twoAdjacent :: String -> Bool
twoAdjacent s =
    s !! 0 == s !! 1 ||
        s !! 1 == s !! 2 ||
            s !! 2 == s !! 3 ||
                s !! 3 == s !! 4 ||
                    s !! 4 == s !! 5

noDecrease :: String -> Bool
noDecrease s =
    s !! 0 <= s !! 1 &&
        s !! 1 <= s !! 2 &&
            s !! 2 <= s !! 3 &&
                s !! 3 <= s !! 4 &&
                    s !! 4 <= s !! 5

main :: IO ()
main = do
    let range = [356261 .. 846303] :: [Int]
    let srange = map show range
    let poss = [x | x <- srange, noDecrease x && twoAdjacent x]
    print $ length poss
