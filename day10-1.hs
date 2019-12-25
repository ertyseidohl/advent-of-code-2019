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

isOnLine :: Asteroid -> Asteroid -> Asteroid -> Bool
isOnLine a b m =
    let
        dot = dotProduct a b m
    in
        crossProduct a b m == 0
        && dot > 0
        && dot <= squaredLength a b

canSee :: [Asteroid] -> Asteroid -> Asteroid -> Bool
canSee as a b = not (any (\ t -> isOnLine a b t && t /= a && t /= b) as)

viewCount :: [Asteroid] -> [Asteroid] -> [(Asteroid, Int)]
viewCount _ [] = []
viewCount allAsteroids (a:as) = (a, viewCount' allAsteroids allAsteroids a) : viewCount allAsteroids as

crossProduct :: Asteroid -> Asteroid -> Asteroid -> Int
crossProduct a b c = (y c - y a) * (x b - x a) - (x c - x a) * (y b - y a)

dotProduct :: Asteroid -> Asteroid -> Asteroid -> Int
dotProduct a b c = (x c - x a) * (x b - x a) + (y c - y a) * (y b - y a)

squaredLength :: Asteroid -> Asteroid -> Int
squaredLength a b = (x b - x a) * (x b - x a) + (y b - y a) * (y b - y a)

viewCount' :: [Asteroid] -> [Asteroid] -> Asteroid -> Int
viewCount' _ [] _ = 0
viewCount' allAsteroids (a:as) t =
    if canSee allAsteroids a t
    then 1 + viewCount' allAsteroids as t
    else viewCount' allAsteroids as t

main :: IO ()
main = do
    input <- getRawInput
    let asteroids = toAsteroids input
    let views = viewCount asteroids asteroids
    let mina = (Asteroid { x = -1, y = -1 }, -1) :: (Asteroid, Int)
    let maxa = foldl (\(a', c') (a, c) -> if c > c' then (a, c) else (a', c')) mina views
    print maxa -- one extra for some reason??
