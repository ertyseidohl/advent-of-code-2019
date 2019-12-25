getRawInput :: IO String
getRawInput = readFile "./day03.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

process :: String -> [[String]]
process raw = map split $ lines raw

data Line = Line {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving (Show)
data Point = Point {px :: Int, py:: Int} deriving (Show)

stringToLine :: String -> Int -> Int -> Line
stringToLine ('U':dy) ix1 iy1 = Line ix1 iy1 ix1 (iy1 - read dy)
stringToLine ('D':dy) ix1 iy1 = Line ix1 iy1 ix1 (iy1 + read dy)
stringToLine ('L':dx) ix1 iy1 = Line ix1 iy1 (ix1 - read dx) iy1
stringToLine ('R':dx) ix1 iy1 = Line ix1 iy1 (ix1 + read dx) iy1
stringToLine s _ _ = error $ "Bad dir: " ++ s

makeLines :: [String] -> [Line]
makeLines ds = makeLines' ds 0 0

makeLines' :: [String] -> Int -> Int -> [Line]
makeLines' [] _ _ = []
makeLines' (d:ds) x y =
    let newLine = stringToLine d x y in
    newLine : makeLines' ds (x2 newLine) (y2 newLine)

isCollision :: Line -> Line -> Bool
isCollision a b
    | x1 a == x2 a = isCollision' a b
    | x1 b == x2 b = isCollision' b a
    | otherwise = False -- parallel

isCollision' :: Line -> Line -> Bool
isCollision' v h =
    (x1 v > x1 h && x1 v < x2 h && y1 h > y1 v && y1 h < y2 v) ||
    (x1 v < x1 h && x1 v > x2 h && y1 h > y1 v && y1 h < y2 v)

collisionPoint :: Line -> Line -> Point
collisionPoint a b = if x1 a == x2 a then Point (x1 a) (y1 b) else Point (x1 b) (y1 a)

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs (px p1 - px p2) + abs (py p1 - py p2)

main :: IO ()
main = do
    raw <- getRawInput
    let processedInput = process raw
    let linesDict = map makeLines processedInput
    let collisions = [ collisionPoint l1 l2 | l1 <- head linesDict, l2 <- linesDict !! 1, isCollision l1 l2 ]
    let points = map (manhattanDistance (Point 0 0)) collisions
    print $ minimum points
