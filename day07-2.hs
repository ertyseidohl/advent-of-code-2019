import Data.List (permutations)
import IntCode (Computer(..), Status (..), err, runInstruction)
import Data.Sequence (Seq(..), update, fromList, (!?))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

getRawInput :: IO String
getRawInput = readFile "./day07.input"

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

genAmpSeq :: [Int] -> [Phase] -> Seq Computer
genAmpSeq intCode phases =
    fromList $
    map
        (\p -> Computer {
            memory = Seq.fromList intCode,
            input = [p],
            output = [],
            ip = 0,
            status = RUNNING,
            compId = p
        })
        phases

getAmps :: [Int] -> [Seq Computer]
getAmps intCode = map (genAmpSeq intCode) allPerms

runUntilBreak :: Computer -> Computer
runUntilBreak c
    | status c == READING = c
    | status c == HALTED = c
    | status c == WRITING = c
    | status c == RUNNING = runUntilBreak $ runInstruction c
    | otherwise = error $ err "Unknown status" c

setInput :: Int -> Int -> Seq Computer -> Seq Computer
setInput index value amps =
    let
        newamp = case amps !? index of
            Nothing -> error $ "Bad setInput amp index: " ++ show index
            Just amp ->
                amp {
                    input = value : input amp
                }
    in
        update index newamp amps

calculateFinalOutput :: Int -> Seq Computer -> Int
calculateFinalOutput index amps =
    let
        ci = (index `mod` 5)
        ni = ((index + 1) `mod` 5)
        curr = case amps !? ci of
            Nothing -> error $ "Bad amp index (curr): " ++ show ci
            Just c -> c
        next = case amps !? ni of
            Nothing -> error $ "Bad amp index (next): " ++ show ni
            Just c -> c
        currRan = runUntilBreak $ runInstruction curr
        nextOut = next {
            input = head (output currRan) : input next
        }
        currRan' = currRan {
            output = []
        }
        updatedAmps' = update ci currRan' amps
        updatedAmps = update ni nextOut updatedAmps'
    in
        if
            status currRan == HALTED
        then
            case input currRan of
                [] -> error $ err "Empty input after halted!" currRan
                x:_ -> x
        else
            calculateFinalOutput (index + 1) updatedAmps

main :: IO ()
main = do
    rawInput <- getRawInput
    let splitInput = split rawInput
    let intCode = map read splitInput :: [Int]
    let ampss = map (fmap runUntilBreak) $ getAmps intCode
    let ampss' = fmap (setInput 0 0) ampss
    let outputs = map (calculateFinalOutput 0) ampss'
    print $ maximum outputs

-- To just run one amp set:
-- let amps = genAmpSeq intCode [9,7,8,5,6]
-- let amps' = fmap runUntilBreak amps
-- let amps'' = setInput 0 0 amps'
-- let outputs = calculateFinalOutput 0 amps''

