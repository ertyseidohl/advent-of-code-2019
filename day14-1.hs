-- Actual sol'n in day14-1b.hs

-- import Debug.Trace (trace)
import Data.Char (isDigit)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

getRawInput :: IO String
getRawInput = readFile "./day14.input"

data Chem = Chem {amt :: Int, name :: String} deriving (Show, Eq)
data Reaction = Reaction {uses :: [Chem], gives :: Chem}

type Store = Map String Int

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

multr :: Int -> Reaction -> Reaction
multr m r =
    r {
        uses = map (multc m) (uses r)
    }

multc :: Int -> Chem -> Chem
multc m c =
    c {
        amt = amt c * m
    }

findProducer :: [Reaction] -> Chem -> Reaction
findProducer rs c
    | name c == "ORE" = Reaction { uses = [], gives = c }
    | otherwise =
        let
            reaction = head $ filter (produces $ name c) rs
            multiplier = max 1 $ amt c `div` amt (gives reaction)
        in
            multr multiplier reaction

rollUpOre :: [Reaction] -> Chem -> Int
rollUpOre rs c = rollUpOre' rs [c] 0 (Map.empty :: Map String Int)

getIngredients :: [Reaction] -> Chem -> Store -> ([Chem], Store)
getIngredients reactions needed store =
    let
        reaction = findProducer reactions needed :: Reaction
        ingredients = uses reaction
        produced = gives reaction
        leftover = amt produced - amt needed
    in
        (ingredients, )


rollUpOre' :: [Reaction] -> [Chem] -> Int -> Store -> Int
rollUpOre' _ [] orecount _ = orecount
rollUpOre' reactions (curr:rest) orecount store
    | (== "ORE") $ name curr = rollUpOre' reactions rest (orecount + amt curr) store
    | otherwise =
        let
            (ingredients, newstore) = getIngredients reactions curr store
        in
            rollUpOre' reactions (rest ++ ingredients) orecount newstore

main :: IO ()
main = do
    input <- getRawInput
    let reactions = map toReaction $ lines input
    let fuel = Chem {amt = 1, name = "FUEL"}
    let result = rollUpOre reactions fuel
    print result

