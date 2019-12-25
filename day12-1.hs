import Debug.Trace (trace)

getRawInput :: IO String
getRawInput = readFile "./day12.input"

data Vector3 = Vector3 {x :: Int, y :: Int, z :: Int} deriving (Eq, Show)
data Moon = Moon {pos :: Vector3, vel :: Vector3} deriving (Show)

toVector3 :: String -> Int
toVector3 s = read $ takeWhile (\q -> q /= ',' && q /= '>') (drop 2 s) :: Int

toMoon :: String -> Moon
toMoon input = Moon {
    pos = Vector3 {
          x = toVector3 $ dropWhile (/= 'x') input
        , y = toVector3 $ dropWhile (/= 'y') input
        , z = toVector3 $ dropWhile (/= 'z') input
    }
    , vel = Vector3 {x = 0, y = 0, z = 0}
}

toMoons :: [String] -> [Moon]
toMoons = map toMoon

step :: [Moon] -> [Moon]
step moons =
    let
        velAdjusted = getVelAdjusted moons moons
    in
        getPosAdjusted velAdjusted

getPosAdjusted :: [Moon] -> [Moon]
getPosAdjusted = map adjustPos

adjustPos :: Moon -> Moon
adjustPos m =
    m {
        pos = addVector (pos m) (vel m)
    }

addVector :: Vector3 -> Vector3 -> Vector3
addVector va vb =
    Vector3 {
          x = x va + x vb
        , y = y va + y vb
        , z = z va + z vb
    }

getVelAdjusted :: [Moon] -> [Moon] -> [Moon]
getVelAdjusted _ [] = []
getVelAdjusted allMoons (m:ms) =
    let adjustment = foldl1 addVector (map (toRelative (pos m) . pos) allMoons)
    in m {
        vel = addVector (vel m) adjustment
    } : getVelAdjusted allMoons ms

toRelative :: Vector3 -> Vector3 -> Vector3
toRelative o a =
    Vector3 {
        x = adjust (x o) (x a)
        , y = adjust (y o) (y a)
        , z = adjust (z o) (z a)
    }

adjust :: Int -> Int -> Int
adjust curr next
    | curr < next = 1
    | curr > next = -1
    | otherwise = 0

showMoons :: [Moon] -> IO ()
showMoons [] = print ""
showMoons (m:ms) = do
    print $ show m
    showMoons ms

energy :: Vector3 -> Int
energy v = abs (x v) + abs (y v) + abs (z v)

totalEnergy :: [Moon] -> Int
totalEnergy = foldr (\ m -> (+) (energy (pos m) * energy (vel m))) 0

runStep :: Int -> [Moon] -> [Moon]
runStep 0 ms = ms
runStep c ms = runStep (c - 1) (step ms)

main :: IO ()
main = do
    input <- getRawInput
    let moons = toMoons $ lines input
    let moons' = runStep 1000 moons
    showMoons moons'
    print $ totalEnergy moons'
