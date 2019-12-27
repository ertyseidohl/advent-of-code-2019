-- import Debug.Trace (trace)
import Data.Char (isDigit)

getRawInput :: IO String
getRawInput = readFile "./day14.input"

data Chem = Chem {amt :: Int, name :: String} deriving (Show, Eq)
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

produces :: String -> Reaction -> Bool
produces s = (== s) .  name . gives

findProducer :: [Reaction] -> Chem -> Reaction
findProducer rs c
    | name c == "ORE" = Reaction { uses = [], gives = c }
    | otherwise = head $ filter (produces $ name c) rs

toRNode :: [Reaction] -> Reaction -> RNode
toRNode rs r =
    RNode {
        chem = gives r
        , children = map (toRNode rs . findProducer rs) (uses r)
    }

-- rollUpOre :: RNode -> Int -> Int
-- rollUpOre rn mult
--     | name (chem rn) == "ORE" = mult * amt (chem rn)
--     | otherwise = rollUpOre

main :: IO ()
main = do
    input <- getRawInput
    let reactions = map toReaction $ lines input
    let fuelReaction = head $ filter (produces "FUEL") reactions
    let reactionTree = toRNode reactions fuelReaction
    print reactionTree
    -- let result = rollUpOre reactionTree 1
    -- print result

