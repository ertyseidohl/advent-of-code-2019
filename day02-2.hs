getRawInput :: IO String
getRawInput = readFile "./day02.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

replace :: [Int] -> Int -> Int -> [Int]
replace mem index new =
    let (start,end) = splitAt index mem in
    start ++ [new] ++ tail end

getInstruction :: Int -> (Int -> Int -> Int)
getInstruction 1 = (+)
getInstruction 2 = (*)
getInstruction x = error $ "Bad instruction " ++ show x

runMachine :: [Int] -> [Int]
runMachine list = runMachine' list 0

runMachine' :: [Int] -> Int -> [Int]
runMachine' mem ip
    | (mem !! ip) == 99 = mem
    | otherwise = do
        let i = mem !! ip
        let x = mem !! (mem !! (ip + 1))
        let y = mem !! (mem !! (ip + 2))
        let o = mem !! (ip + 3)
        let inst = getInstruction i
        let newmem = replace mem o (inst x y)
        runMachine' newmem (ip + 4)

tryUntilSuccess :: [Int] -> [(Int, Int)] -> Int -> [Int]
tryUntilSuccess _ [] _ = error "Ran out of tries"
tryUntilSuccess intCode (try:tries) target = do
    let modifiedIntCode = [head intCode] ++ [fst try] ++ [snd try] ++ drop 3 intCode
    let result = runMachine modifiedIntCode
    if head result == target then result else tryUntilSuccess intCode tries target

main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]
    let nouns = [0..99] :: [Int]
    let verbs = [0..99] :: [Int]
    let tries = [ (n, v) | n <- nouns, v <- verbs ]
    let result = tryUntilSuccess intCode tries 19690720
    print $ show (result !! 1) ++ show (result !! 2)
