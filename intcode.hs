module IntCode (runMachine, Computer (..), Status (..), err, runInstruction) where

import Debug.Trace (trace)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe (fromMaybe)

debug :: Bool
debug = False

type Memory = IntMap Int
type Input = [Int]
type Output =  [Int]
type IP = Int

data IOType = INPUT | OUTPUT deriving (Eq)
data Mode = IMMEDIATE | POSITION | RELATIVE deriving (Eq, Show)
data Status = RUNNING | READING | WRITING | HALTED deriving (Eq, Show)

data Computer = Computer {
    memory :: Memory
    , ip :: IP
    , input :: Input
    , output :: Output
    , status :: Status
    , compId :: Int
    , relativeBase :: Int} deriving (Show)

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
toMode 0 = POSITION
toMode 1 = IMMEDIATE
toMode 2 = RELATIVE
toMode x = error $ "Unknown mode " ++ show x

getValue :: Computer -> Mode -> Int -> Int
getValue c mode index
    | mode == POSITION = case memory c IntMap.!? index of
        Nothing -> error $ err ("Bad POSITION index: " ++ show index) c
        Just x -> fromMaybe 0 (memory c IntMap.!? x)
    | mode == IMMEDIATE = case memory c IntMap.!? index of
        Nothing -> error $ err ("Bad IMMEDIATE index: " ++ show index) c
        Just x -> x
    | mode == RELATIVE = case memory c IntMap.!? index of
        Nothing -> error $ err ("Bad RELATIVE index: " ++ show index) c
        Just x -> fromMaybe 0 (memory c IntMap.!? (x + relativeBase c))
    | otherwise = error $ err "Illegal mode in getValue" c

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
                    9 -> rel inst c
                    99 -> c {status = HALTED}
                    _ -> error $ err "Bad instruction." c
            in
                if debug
                then
                    if i inst == 3
                    then trace (show c) trace (show newC) newC
                    else trace (show newC) newC
                else newC

rel :: Instruction -> Computer -> Computer
rel inst c =
    let
        adj = getValue c (m0 inst) (ip c + 1)
    in
        c {
            ip = ip c + 2
            , relativeBase = relativeBase c + adj
        }

math :: (Int -> Int -> Int) -> Instruction -> Computer -> Computer
math op inst c =
    let
        x = getValue c (m0 inst) (ip c + 1)
        y = getValue c (m1 inst) (ip c + 2)
        o = getValue c IMMEDIATE (ip c + 3)
        o' = if m2 inst == RELATIVE then o + relativeBase c else o
    in
        c {
            memory = IntMap.insert o' (x `op` y) (memory c)
            , ip = ip c + 4
            , status = RUNNING
        }

cio :: IOType -> Instruction -> Computer -> Computer
cio iotype inst c =
    case iotype of
        INPUT -> let
                il = getValue c IMMEDIATE (ip c + 1)
                ii = if m0 inst == RELATIVE then il + relativeBase c else il
            in
                case input c of
                    [] -> c {
                        status = READING
                    }
                    xs -> c {
                        memory = IntMap.insert ii (head xs) (memory c)
                        , ip = ip c + 2
                        , input = tail xs
                        , status = RUNNING
                    }
        OUTPUT -> let
                ol = getValue c (m0 inst) (ip c + 1)
            in
                c {
                    ip = ip c + 2
                    , output = ol : output c
                    , status = WRITING
                }

jump :: (Int -> Bool) -> Instruction -> Computer -> Computer
jump test inst c =
    let
        toTest = getValue c (m0 inst) (ip c + 1)
    in
        if test toTest
        then
            c {
                ip = getValue c (m1 inst) (ip c + 2)
                , status = RUNNING
            }
        else
            c {
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
            , compId = 0
            , relativeBase = 0
        }
    in
        runMachine' c
