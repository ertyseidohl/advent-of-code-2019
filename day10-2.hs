import Data.List (sortBy)

getRawInput :: IO String
getRawInput = readFile "./day10.input"

data Asteroid = Asteroid {x :: Int, y :: Int} deriving (Show, Eq)

toAsteroids' :: Int -> Int -> String -> [Asteroid]
toAsteroids' _ _ [] = []
toAsteroids' xi yi (s:ss)
    | s == '\n' = toAsteroids' 0 (yi + 1) ss
    | s == '.' = toAsteroids' (xi + 1) yi ss
    | s == '#' = Asteroid xi yi : toAsteroids' (xi + 1) yi ss
    | otherwise = error $ "Bad string?" ++ (s:ss)

toAsteroids :: String -> [Asteroid]
toAsteroids = toAsteroids' 0 0

angle :: Asteroid -> Asteroid -> Double
angle a b =
    let
        yd = fromIntegral (y a - y b) :: Double
        xd = fromIntegral (x a - x b) :: Double
    in
        atan2 yd xd

calcAngle :: Asteroid -> Asteroid -> (Asteroid, Double)
calcAngle l a = (a, angle l a)

sortByAngle :: (Asteroid, Double) -> (Asteroid, Double) -> Ordering
sortByAngle a b | snd a == snd b = EQ
                | snd a > snd b = GT
                | otherwise = LT

approx :: Double -> Double -> Bool
approx a b = take 6 (show a) == take 6 (show b) -- lol

laserBlast :: Int -> [(Asteroid, Double)] -> [(Asteroid, Double)]
laserBlast 0 [] = error "no remaining asteroids, but hit 0 to blast!"
laserBlast 0 (a:_) = [a]
laserBlast r [] = error $ "no remaining asteroids, want to blast " ++ show r ++ " more!"
laserBlast i as@(a:rest) =
    let
        sameAngle (_, ang) = ang `approx` snd a
        inALine = takeWhile sameAngle as
    in
        laserBlast (i - 1) (dropWhile sameAngle rest ++ inALine)

pointLaserUp :: [(Asteroid, Double)] -> [(Asteroid, Double)]
pointLaserUp as =
    let
        isToTheLeft (_, ang) = ang < pi / 2
    in
        dropWhile isToTheLeft as ++ takeWhile isToTheLeft as

main :: IO ()
main = do
    input <- getRawInput
    let asteroids = toAsteroids input
    let laser = Asteroid 19 14
    let angles = map (calcAngle laser) asteroids
    let sortedAngles = sortBy sortByAngle angles
    let sortedAnglesWithLaserPointingUp = pointLaserUp sortedAngles
    let blasted = laserBlast 199 sortedAnglesWithLaserPointingUp
    print blasted

-- Station is at (19,14)
