-- import Debug.Trace (trace)
import Data.Char (isDigit)
import Data.List

getRawInput :: IO String
getRawInput = readFile "./day14.input"

data Chem = Chem {amt :: Int, name :: String}
data Reaction = Reaction {uses :: [Chem], gives :: Chem}
data RNode = RNode {chem :: Chem, children :: [RNode]} deriving (Show)

instance Show Reaction where
    show r = intercalate ", " (map show $ uses r) ++ " => " ++ show (gives r)

instance Show Chem where
    show c = show (amt c) ++ " " ++ name c

split :: Char -> String -> [String]
split _ "" = []
split c s = firstword : split c rest
    where firstword = takeWhile (/=c) s
          rest = drop (length firstword + 1) s

toChem :: String -> Chem
toChem (' ':rest) = toChem rest
toChem s =
    let
        a = read $ takeWhile isDigit s :: Int
        n = filter (/= ' ') $ dropWhile isDigit s
    in Chem {amt = a, name = n }


toReaction :: String -> Reaction
toReaction s =
    let
        u = map toChem (split ',' (takeWhile (/='=') s))
        g = toChem $ tail (dropWhile (/='>') s)
    in Reaction { uses = u, gives = g }

producesr :: String -> Reaction -> Bool
producesr s = (== s) .  name . gives

findProducer :: [Reaction] -> Chem -> Reaction
findProducer rs c = head $ filter (producesr $ name c) rs

usesOre :: Reaction -> Bool
usesOre r = isOre $ head (uses r)

usesHowMuchOre :: Reaction -> Int
usesHowMuchOre r = amt $ head (uses r)

isOre :: Chem -> Bool
isOre c = (== "ORE") $ name c

toTree :: [Reaction] -> Chem -> RNode
toTree rs c =
    let
        producer = findProducer rs c :: Reaction
        amountProduced = fromIntegral $ amt (gives producer) :: Float
        amountNeeded = fromIntegral (amt c) :: Float
        multiplier = ceiling $ amountNeeded / amountProduced :: Int
        used = filter (not . isOre) $ uses producer :: [Chem]
        usedmultiplied = map (\u -> u {amt = amt u * multiplier}) used
    in
        RNode {chem = c, children = map (toTree rs) usedmultiplied}

getTotal :: String -> RNode  -> Int
getTotal s n
    | name (chem n) == s = amt $ chem n
    | otherwise = sum $ map (getTotal s) (children n)

rollUpOre :: RNode -> Reaction -> Int
rollUpOre node curr =
    let
        totalNeeded = fromIntegral $ getTotal (name $ gives curr) node :: Float
        totalGiven =  fromIntegral (amt $ gives curr) :: Float
    in
        ceiling (totalNeeded / totalGiven)

main :: IO ()
main = do
    input <- getRawInput
    let reactions = map toReaction $ lines input
    let oreReactions = filter usesOre reactions
    let reactionTree = toTree reactions Chem{name="FUEL", amt=1}
    print reactionTree
    let oreCounts = map (rollUpOre reactionTree) oreReactions
    let total = sum $ zipWith (*) oreCounts (map usesHowMuchOre oreReactions)
    print total
