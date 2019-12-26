import IntCode (runMachine)

getRawInput :: IO String
getRawInput = readFile "./day13.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

countBlocks :: [Int] -> Int
countBlocks [] = 0
countBlocks (_:_:2:rest) = 1 + countBlocks rest
countBlocks (_:_:_:rest) = countBlocks rest


main :: IO ()
main = do
    rawInput <- getRawInput
    let splitInput = split rawInput
    let intCode = map read splitInput :: [Int]
    let gameScreen = reverse $ snd (runMachine intCode [])
    print $ countBlocks gameScreen
