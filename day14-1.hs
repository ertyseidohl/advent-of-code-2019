import Debug.Trace (trace)
import Data.Char (isDigit)

getRawInput :: IO String
getRawInput = readFile "./day14.input"

data Chem = Chem {amt :: Int, name :: String} deriving (Eq, Show)
data Reaction = Reaction {uses :: [Chem], gives :: Chem}
data RNode = RNode {chem :: Chem, children :: [RNode]} deriving (Show)

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

isOre :: Chem -> Bool
isOre c = (== "ORE") $ name c

toTree :: [Reaction] -> Reaction -> RNode
toTree rs r =
    let
        c = gives r :: Chem
        u = uses r :: [Chem]
        p = map (findProducer rs) (filter (not . isOre) u) :: [Reaction]
        ch = map (toTree rs) p :: [RNode]
    in
        RNode {chem = c, children = ch}

getTotal :: String -> RNode  -> Int
getTotal s n
    | name (chem n) == s = amt $ chem n
    | otherwise = sum $ map (getTotal s) (children n)

rollUpOre :: RNode -> Reaction -> Int
rollUpOre n curr =
    let
        totalNeeded = fromIntegral $ getTotal (name $ gives curr) n :: Float
        totalGiven =  fromIntegral (amt $ gives curr) :: Float
    in
        trace (show (totalNeeded, totalGiven)) $ ceiling (totalNeeded / totalGiven)


main :: IO ()
main = do
    input <- getRawInput
    let reactions = map toReaction $ lines input
    let fuelReaction = head $ filter (producesr "FUEL") reactions
    let oreReactions = filter usesOre reactions
    let reactionTree = toTree reactions fuelReaction
    print reactionTree
    let oreCounts = map (rollUpOre reactionTree) oreReactions
    print oreCounts
