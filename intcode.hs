module IntCode (runMachine) where

import Debug.Trace (trace)
import Data.Sequence (Seq(..), index, update, fromList, (!?))
import qualified Data.Sequence as Seq
import Data.List (unfoldr)
import Data.Tuple (swap)

debug :: Bool
debug = True

type Memory = Seq Int
type Input = [Int]
type Output =  [Int]
type IP = Int

data IOType = INPUT | OUTPUT deriving (Eq)
data Mode = IMMEDIATE | POSITION deriving (Eq, Show)

data Computer = Computer {
    memory :: Memory,
    ip :: IP,
    input :: Input,
    output :: Output } deriving (Show)

data Instruction = Instruction {
    i :: Int,
    m0 :: Mode,
    m1 :: Mode,
    m2 :: Mode
    } deriving (Show)

err :: String -> Computer -> String
err s c = s ++ " IP: " ++ show (ip c) ++ " mem: " ++ show (memory c)

digits :: Int -> Seq Int
digits d = pad 5 $ digits' d

pad :: Int -> Seq Int -> Seq Int
pad l xs = if length xs == l then xs else pad l (0 Seq.<| xs)

digits' :: Int -> Seq Int
digits' = fromList . reverse . unfoldr (\x -> if x == 0 then Nothing else Just $ swap (divMod x 10))

toInstruction :: Int -> Instruction
toInstruction inst = let
    d = digits inst
    in Instruction (inst `mod` 100) (toMode $ index d 2) (toMode $ index d 1) (toMode $ index d 0)

toMode :: Int -> Mode
toMode int = if int == 1 then IMMEDIATE else POSITION

runInstruction :: Computer -> Computer
runInstruction c =
    case memory c !? ip c of
        Nothing -> error $ err "Bad instruction lookup." c
        Just l -> let inst = toInstruction l in
            case i inst of
                1 -> math (+) inst c
                2 -> math (*) inst c
                3 -> cio INPUT inst c
                4 -> cio OUTPUT inst c
                5 -> jump (/= 0) inst c
                6 -> jump (== 0) inst c
                7 -> math (\x y -> if x < y then 1 else 0) inst c
                8 -> math (\x y -> if x == y then 1 else 0) inst c
                _ -> error $ err "Bad instruction." c

update' :: Int -> Int -> Seq Int -> Seq Int
update' ix e s = if ix < length s then update ix e s else update' ix e (s Seq.|> 0)

math :: (Int -> Int -> Int) -> Instruction -> Computer -> Computer
math op inst c
    | length (memory c) < ip c + 3 = error $ err "Out of range math." c
    | otherwise =
        let
            ipc = ip c
            x = index (memory c) (ipc + 1)
            y = index (memory c) (ipc + 2)
            o = index (memory c) (ipc + 3)
        in
            let
                xv = if m0 inst == IMMEDIATE then x else index (memory c) x
                yv = if m1 inst == IMMEDIATE then y else index (memory c) y
            in
                trace (show xv ++ ", " ++ show yv ++ ", " ++ show (xv `op` yv)) $
                c {
                    memory = update' o (xv `op` yv) (memory c),
                    ip = ipc + 4
                }

cio :: IOType -> Instruction -> Computer -> Computer
cio iotype inst c =
    case iotype of
        INPUT -> case memory c !? (ip c + 1) of
            Nothing -> error $ err "Bad INPUT instruction." c
            Just il ->
                case input c of
                    [] -> error $ err "Reading INPUT from empty list." c
                    xs -> c {
                        memory = update' il (head xs) (memory c),
                        ip = ip c + 2,
                        input = tail xs
                    }
        OUTPUT -> case memory c !? (ip c + 1) of
            Nothing -> error $ err "Bad OUTPUT instruction." c
            Just ol -> c {
                ip = ip c + 2,
                output = if m0 inst == IMMEDIATE
                    then ol : output c
                    else case memory c !? ol of
                        Nothing -> error $ err "Bad OUTPUT origin." c
                        Just x -> x : output c
            }

jump :: (Int -> Bool) -> Instruction -> Computer -> Computer
jump test inst c =
    case memory c !? (ip c + 1) of
        Nothing -> error $ err "Bad jump first input." c
        Just toTest -> if test (
                if m0 inst == IMMEDIATE
                then toTest
                else case memory c !? toTest of
                    Nothing -> error $ err "Bad test memory location" c
                    Just loadedTest -> loadedTest
            )
            then
                c {
                    ip = case memory c !? (ip c + 2) of
                        Nothing -> error $ err "Bad jump second input." c
                        Just toJump -> if m1 inst == IMMEDIATE
                            then toJump
                            else case memory c !? toJump of
                                Nothing -> error $ err "Bad jump target." c
                                Just jumpLoc -> jumpLoc
                }
            else c {
                ip = ip c + 3
            }



runMachine' :: Bool -> Computer -> (Int, Output)
runMachine' trace' c =
    case memory c !? ip c of
        Nothing -> error $ err "Bad instruction index." c
        Just 99 ->
            let
                zeroth = case memory c !? 0 of
                    Nothing -> error $ err "Empty memory." c
                    Just x -> x
                output' = output c
            in
                if trace'
                then trace (show c) (zeroth, output')
                else (zeroth, output')
        Just _ ->
            if trace'
            then runMachine' trace' $ trace (show c) $ runInstruction c
            else runMachine' trace' $ runInstruction c

-- Takes in initial memory and input, returns ending first cell + output
runMachine :: [Int] -> [Int] -> (Int, [Int])
runMachine initMem input' =
    let
        c = Computer {
            memory = Seq.fromList initMem,
            input = input',
            output = [],
            ip = 0
        }
    in
        runMachine' debug c
