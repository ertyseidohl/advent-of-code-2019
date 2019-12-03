getRawInput :: IO String
getRawInput = readFile "./day3.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

process :: String -> [[String]]
process raw = map split $ lines raw

data Line = Line {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving (Show)

stringToLine :: String -> Int -> Int -> Line
stringToLine ('U':dy) ix1 iy1 = Line ix1 iy1 ix1 (iy1 - read dy)
stringToLine ('D':dy) ix1 iy1 = Line ix1 iy1 ix1 (iy1 + read dy)
stringToLine ('L':dx) ix1 iy1 = Line ix1 iy1 (ix1 - read dx) iy1
stringToLine ('R':dx) ix1 iy1 = Line ix1 iy1 (ix1 + read dx) iy1
stringToLine x _ _ = error $ "Bad dir: " ++ x

makeLines :: [String] -> [Line]
makeLines ds = makeLines' ds 0 0

makeLines' :: [String] -> Int -> Int -> [Line]
makeLines' [] _ _ = []
makeLines' (d:ds) x y =
    let newLine = stringToLine d x y in
    newLine : makeLines' ds (x2 newLine) (y2 newLine)

isCollision :: Line -> Line -> Bool
-- I think this isn't working
isCollision v h = (x1 v > x1 h) && (x1 v < x2 h) && (y1 h > y1 v) && (y2 h < y2 v)

main :: IO ()
main = do
    raw <- getRawInput
    let processedInput = process raw
    let linesDict = map makeLines processedInput
    let collisions = [ if isCollision l1 l2 then ([l1, l2], True) else ([l1, l2], False) | l1 <- head linesDict, l2 <- linesDict !! 1 ]
    print collisions
