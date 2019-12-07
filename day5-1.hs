import intcode.intcode

getRawInput :: IO String
getRawInput = readFile "./day2.input"

main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]

