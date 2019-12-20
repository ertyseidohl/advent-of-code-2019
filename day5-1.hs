import IntCode (runMachine)

getRawInput :: IO String
getRawInput = readFile "./day5.input"

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
    let (zero, out) = runMachine intCode [5]
    print zero
    print out
