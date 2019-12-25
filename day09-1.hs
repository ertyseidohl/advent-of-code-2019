import IntCode (runMachine)

getRawInput :: IO String
getRawInput = readFile "./day09.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]
    let (zero, out) = runMachine intCode [1]
    print zero
    print out
