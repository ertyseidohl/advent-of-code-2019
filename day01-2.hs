fuel :: Int -> Int
fuel mass = div mass 3 - 2

fuel' :: Int -> Int
fuel' f = fuelcalc (0, f)

fuelcalc :: (Int, Int) -> Int
fuelcalc (s, c) = do
    let nx = fuel c
    if nx <= 0
        then s
        else fuelcalc (s + nx, nx)


getRawInput :: IO String
getRawInput = readFile "./day01.input"

getModules :: String -> [Int]
getModules raw = do
    let l = lines raw
    map read l :: [Int]

main :: IO ()
main = do
    raw <- getRawInput
    let modules = getModules raw
    let total = sum $ map fuel' modules
    print . show $ total
