collect :: [Int] -> Int -> [Int]
collect xs i =
    start ++ [(xs !! i) + 1] ++ tail end
    where (start,end) = splitAt i xs

containsDouble :: [Int] -> Bool
containsDouble [] = False
-- "Redundant if" apparently
containsDouble (x:xs) = if x == 2 then True else containsDouble xs

check :: String -> Bool
check s = containsDouble (toCounts s [0,0,0,0,0,0,0,0,0,0])

toCounts :: String -> [Int] -> [Int]
-- Should use foldl here
toCounts [] c = c
toCounts (s:ss) c =
    toCounts ss (collect c (read [s] :: Int))

noDecrease :: String -> Bool
noDecrease s =
    head s <= s !! 1 &&
        s !! 1 <= s !! 2 &&
            s !! 2 <= s !! 3 &&
                s !! 3 <= s !! 4 &&
                    s !! 4 <= s !! 5

main :: IO ()
main = do
    let range = [356261 .. 846303] :: [Int]
    let srange = map show range
    let poss = [x | x <- srange, noDecrease x && check x]
    print $ length poss
