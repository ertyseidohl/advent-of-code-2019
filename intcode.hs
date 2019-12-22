module IntCode (runMachine, Computer (..), Status (..), err, runInstruction) where

import Debug.Trace (trace)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

debug :: Bool
debug = False

type Memory = IntMap Int
type Input = [Int]
type Output =  [Int]
type IP = Int

data IOType = INPUT | OUTPUT deriving (Eq)
data Mode = IMMEDIATE | POSITION deriving (Eq, Show)
data Status = RUNNING | READING | WRITING | HALTED deriving (Eq, Show)

data Computer = Computer {
    memory :: Memory,
    ip :: IP,
    input :: Input,
    output :: Output,
    status :: Status,
    phase :: Int } deriving (Show)

data Instruction = Instruction {
    i :: Int,
    m0 :: Mode,
    m1 :: Mode,
    m2 :: Mode
    } deriving (Show)

err :: String -> Computer -> String
err s c = s ++ " " ++ show c

digits :: Int -> Seq Int
digits d = pad 5 $ digits' d

pad :: Int -> Seq Int -> Seq Int
pad l xs = if length xs == l then xs else pad l (0 Seq.<| xs)

digits' :: Int -> Seq Int
digits' = Seq.fromList . reverse . unfoldr (\x -> if x == 0 then Nothing else Just $ swap (divMod x 10))

toInstruction :: Int -> Instruction
toInstruction inst = let
    d = digits inst
    in Instruction (inst `mod` 100) (toMode $ Seq.index d 2) (toMode $ Seq.index d 1) (toMode $ Seq.index d 0)

toMode :: Int -> Mode
toMode int = if int == 1 then IMMEDIATE else POSITION

runInstruction :: Computer -> Computer
runInstruction c =
    case memory c IntMap.!? ip c of
        Nothing -> error $ err "Bad instruction lookup." c
        Just l -> let inst = toInstruction l in
            let newC = case i inst of
                    1 -> math (+) inst c
                    2 -> math (*) inst c
                    3 -> cio INPUT inst c
                    4 -> cio OUTPUT inst c
                    5 -> jump (/= 0) inst c
                    6 -> jump (== 0) inst c
                    7 -> math (\x y -> if x < y then 1 else 0) inst c
                    8 -> math (\x y -> if x == y then 1 else 0) inst c
                    99 -> c {status = HALTED}
                    _ -> error $ err "Bad instruction." c
            in
                if debug
                then
                    if i inst == 3
                    then trace (show c) trace (show newC) newC
                    else trace (show newC) newC
                else newC

math :: (Int -> Int -> Int) -> Instruction -> Computer -> Computer
math op inst c
    | length (memory c) < ip c + 3 = error $ err "Out of range math." c
    | otherwise =
        let
            ipc = ip c
            x = memory c IntMap.! (ipc + 1)
            y = memory c IntMap.! (ipc + 2)
            o = memory c IntMap.! (ipc + 3)
        in
            let
                xv = if m0 inst == IMMEDIATE then x else memory c IntMap.! x
                yv = if m1 inst == IMMEDIATE then y else memory c IntMap.! y
            in
                c {
                    memory = IntMap.insert o (xv `op` yv) (memory c)
                    , ip = ipc + 4
                    , status = RUNNING
                }

cio :: IOType -> Instruction -> Computer -> Computer
cio iotype inst c =
    case iotype of
        INPUT -> case memory c IntMap.!? (ip c + 1) of
            Nothing -> error $ err "Bad INPUT instruction." c
            Just il ->
                case input c of
                    [] -> c {
                        status = READING
                    }
                    xs -> c {
                        memory = IntMap.insert il (head xs) (memory c)
                        , ip = ip c + 2
                        , input = tail xs
                        , status = RUNNING
                    }
        OUTPUT -> case memory c IntMap.!? (ip c + 1) of
            Nothing -> error $ err "Bad OUTPUT instruction." c
            Just ol -> c {
                ip = ip c + 2
                , output = if m0 inst == IMMEDIATE
                    then ol : output c
                    else case memory c IntMap.!? ol of
                        Nothing -> error $ err "Bad OUTPUT origin." c
                        Just x -> x : output c
                , status = WRITING
            }

jump :: (Int -> Bool) -> Instruction -> Computer -> Computer
jump test inst c =
    case memory c IntMap.!? (ip c + 1) of
        Nothing -> error $ err "Bad jump first input." c
        Just toTest -> if test (
                if m0 inst == IMMEDIATE
                then toTest
                else case memory c IntMap.!? toTest of
                    Nothing -> error $ err "Bad test memory location" c
                    Just loadedTest -> loadedTest
            )
            then
                c {
                    ip = case memory c IntMap.!? (ip c + 2) of
                        Nothing -> error $ err "Bad jump second input." c
                        Just toJump -> if m1 inst == IMMEDIATE
                            then toJump
                            else case memory c IntMap.!? toJump of
                                Nothing -> error $ err "Bad jump target." c
                                Just jumpLoc -> jumpLoc
                    , status = RUNNING
                }
            else c {
                ip = ip c + 3
                , status = RUNNING
            }


runMachine' :: Computer -> (Int, Output)
runMachine' c =
    let
        newC = runInstruction c
    in case status newC of
        HALTED ->
            let
                zeroth = case memory c IntMap.!? 0 of
                    Nothing -> error $ err "Empty memory." c
                    Just x -> x
                output' = output c
            in (zeroth, output')
        _ -> runMachine' newC

-- Takes in initial memory and input, returns ending first cell + output
runMachine :: [Int] -> [Int] -> (Int, [Int])
runMachine initMem input' =
    let
        c = Computer {
            memory = IntMap.fromList $ zip [0..] initMem
            , input = input'
            , output = []
            , ip = 0
            , status = RUNNING
            , phase = 0
        }
    in
        runMachine' c
