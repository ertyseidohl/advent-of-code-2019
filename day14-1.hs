import Debug.Trace (trace)
import Data.Char (isDigit)

getRawInput :: IO String
getRawInput = readFile "./day14.input"

data Chem = Chem {amt :: Int, name :: String}
data Reaction = Reaction {uses :: [Chem], gives :: Chem}
data RNode = RNode {chem :: Chem, children :: [Rnode]}

split :: Char -> String -> [String]
split _ "" = []
split c s = firstword : split c rest
    where firstword = takeWhile (/=c) s
          rest = drop (length firstword + 1) s

toChem :: String -> Chem
toChem (' ':rest) = toChem rest
toChem s =
    let
        amount = read $ takeWhile isDigit s :: Int
        name = filter (/= ' ') $ dropWhile isDigit s
    in Chem {amt = amount, name = name }


toReaction :: String -> Reaction
toReaction s =
    let
        uses = map toChem (split ',' (takeWhile (/='=') s))
        gives = toChem $ tail (dropWhile (/='>') s)
    in Reaction { uses = uses, gives = gives }

producesr :: String -> Reaction -> Bool
producesr s = (== s) .  name . gives

findProducer :: [Reaction] -> Chem -> Reaction
findProducer rs c = head $ filter (producesr $ name c) rs

toTree :: [Reaction] -> Reaction -> RNode
toTree rs root =


main :: IO ()
main = do
    input <- getRawInput
    let reactions = map toReaction $ lines input
    let fuelReaction = head . filter (producesr "FUEL") reactions
    let otherReactions = filter (not . producesr "FUEL") reactions
    reactionTree = toTree reactions fuelReaction
    print oreAmount

