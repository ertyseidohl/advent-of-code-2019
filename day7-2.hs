import Data.List (permutations)
import IntCode (yieldMachine, Computer)
import Data.Sequence (Seq(..), index, update, fromList, (!?))
import qualified Data.Sequence as Seq

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

allPerms :: [[Phase]]
allPerms = permutations [5..9]

getAmps :: [Int] -> Seq yieldMachine
getAmps intCode = map (yieldMachine intCode) allPerms



main :: IO ()
main = do
    input <- getRawInput
    let splitInput = split input
    let intCode = map read splitInput :: [Int]
    let amps = getAmps intCode
    print $ show $ take 3 amps



