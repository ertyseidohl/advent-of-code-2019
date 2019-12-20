import Data.List (permutations)
import IntCode (runMachine)

getRawInput :: IO String
getRawInput = readFile "./day7.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

type InputSignal = Int
type OutputSignal = Int
type Phase = Int

ampRunner :: [Int] -> Phase -> InputSignal -> OutputSignal
ampRunner intCode phase input =
    let
        output = runMachine intCode [phase, input]
    in head $ snd output

allPerms :: [[Phase]]
allPerms = permutations [5..9]

runAmps :: [InputSignal -> OutputSignal] -> OutputSignal
runAmps = foldr (\a i -> a i) 0

maxAmpOutput :: (Phase -> InputSignal -> OutputSignal) -> OutputSignal
maxAmpOutput ampCode =
    let
        perms = allPerms
        allAmps = map (map ampCode) perms
    in
        maximum $ map runAmps allAmps

main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]
    let maxOut = maxAmpOutput (ampRunner intCode)
    print maxOut



