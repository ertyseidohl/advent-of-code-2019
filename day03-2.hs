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
data Point = Point {px :: Int, py :: Int} deriving (Show)
data Wire = Wire {line :: Line, dist :: Int} deriving (Show)
data Collision = Collision {w1 :: Wire, w2 :: Wire, pt :: Point} deriving (Show)

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

isHorizontal :: Line -> Bool
isHorizontal l = y1 l == y2 l

isCollision :: Line -> Line -> Bool
isCollision a b
    | isHorizontal b = isCollision' a b
    | isHorizontal a = isCollision' b a
    | otherwise = False

isCollision' :: Line -> Line -> Bool
isCollision' v h =
    (x1 v > x1 h && x1 v < x2 h && y1 h > y1 v && y1 h < y2 v) ||
    (x1 v < x1 h && x1 v > x2 h && y1 h > y1 v && y1 h < y2 v) ||
    (x1 v > x1 h && x1 v < x2 h && y1 h < y1 v && y1 h > y2 v) ||
    (x1 v < x1 h && x1 v > x2 h && y1 h < y1 v && y1 h > y2 v)


collisionPoint :: Line -> Line -> Point
collisionPoint a b
    | isHorizontal b = collisionPoint' a b
    | isHorizontal a = collisionPoint' b a
    | otherwise = error "what"

collisionPoint' :: Line -> Line -> Point
collisionPoint' v h = Point (x1 v) (y1 h)

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs (px p1 - px p2) + abs (py p1 - py p2)

lineLength :: Line -> Int
lineLength l = manhattanDistance (Point (x1 l) (y1 l)) (Point (x2 l) (y2 l))

linesToWires :: [Line] -> [Wire]
linesToWires ls = linesToWires' ls 0

linesToWires' :: [Line] -> Int -> [Wire]
linesToWires' [] _ = []
linesToWires' (l:ls) distSoFar = Wire l distSoFar : linesToWires' ls (distSoFar + lineLength l)

getDistanceAtCollision :: Collision -> Int
getDistanceAtCollision c =
    manhattanDistance (Point (x1 hline) (y1 hline)) (pt c) +
        manhattanDistance (Point (x1 vline) (y1 vline)) (pt c) +
            hdist + vdist
    where
        (hw, vw) = if isHorizontal $ line $ w1 c then (w1 c, w2 c) else (w2 c, w1 c)
        hline = line hw
        vline = line vw
        hdist = dist hw
        vdist = dist vw

main :: IO ()
main = do
    raw <- getRawInput
    let processedInput = process raw
    let linesList = map makeLines processedInput
    let wiresList = map linesToWires linesList
    let collisions = [ Collision cw1 cw2 $ collisionPoint (line cw1) (line cw2)
            | cw1 <- head wiresList, cw2 <- wiresList !! 1, isCollision (line cw1) (line cw2) ]
    let results = map getDistanceAtCollision collisions
    print $ head wiresList
    print $ wiresList !! 1
    print collisions
    print results
    print $ minimum results
    -- let t1 = Wire {line = Line {x1 = 158, y1 = 30, x2 = 158, y2 = -53}, dist = 188}
    -- let t2 = Wire {line = Line {x1 = 155, y1 = 12, x2 = 238, y2 = 12}, dist = 401}
    -- print $ collisionPoint (line t1) (line t2)
    -- print $ isCollision (line t2) (line t1)
    -- let c = collisionPoint (line w2) (line w1)
    -- let md1 = manhattanDistance (Point (x1 $ line w1) (y1 $ line w1)) c
    -- let md2 = manhattanDistance (Point (x1 $ line w2) (y1 $ line w2)) c
    -- print (md1, md2)
    -- print (dist w1, dist w2)
    -- print $ getDistanceAtCollision (w1, w2, c)
