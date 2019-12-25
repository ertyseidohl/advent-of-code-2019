getRawInput :: IO String
getRawInput = readFile "./day02.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

perform :: (Int -> Int -> Int ) -> [Int] -> Int -> Int -> Int -> [Int]
perform op list x y o = do
    let (start,end) = splitAt o list
    let res = (list !! x) `op` (list !! y)
    start ++ [res] ++ tail end

getInstruction :: Int -> ([Int] -> Int -> Int -> Int -> [Int])
getInstruction 1 = perform (+)
getInstruction 2 = perform (*)
getInstruction x = error $ "Bad Instruction: " ++ show x

runIntCode :: [Int] -> Int -> Int
runIntCode list index
    | list !! index  == 99 = head list
    | otherwise = do
        let instr = getInstruction $ list !! index
        let result = instr list (list !! (index + 1)) (list !! (index + 2)) (list !! (index + 3))
        runIntCode result (index + 4)

main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]
    let result = runIntCode intCode 0
    print result
