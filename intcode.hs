module IntCode (runMachine) where

import Data.Sequence (Seq(..), index, insertAt)
import qualified Data.Sequence as Seq

type Memory = Seq Int
type Input = [Int]
type Output =  [Int]
type IP = Int

data IOType = INPUT | OUTPUT deriving (Eq)

data Computer = Computer {
    memory :: Memory,
    ip :: IP,
    input :: Input,
    output :: Output } deriving (Show)

runInstruction :: Computer -> Computer
runInstruction c =
    let
        i = index (memory c) (ip c)
    in case i of
        1 -> math (+) c
        2 -> math (*) c
        3 -> cio INPUT c
        4 -> cio OUTPUT c
        x -> error $ "Bad instruction " ++ show x

math :: (Int -> Int -> Int) -> Computer -> Computer
math op c
    | length (memory c) < ip c + 3 = error $ "Out of range math. ip = " ++ show (ip c)
    | otherwise =
        let
            ipc = ip c
            x = index (memory c) (ipc + 1)
            y = index (memory c) (ipc + 2)
            o = index (memory c) (ipc + 3)
        in
            c {
                memory = insertAt o (x `op` y) (memory c),
                ip = ipc + 4
            }

cio :: IOType -> Computer -> Computer
cio i c
    | length (memory c) < ip c + 1 = error $ "Out of range io. ip = " ++ show (ip c)
    | i == INPUT =
        let
            ipc = ip c
            o = index (memory c) (ipc + 1)
        in
            c {
                memory = insertAt o (head $ input c) (memory c),
                ip = ipc + 2,
                input = tail $ input c
            }
    | i == OUTPUT =
        let
            ipc = ip c
            o = index (memory c) (ipc + 1)
        in
            c {
                ip = ipc + 2,
                output = index (memory c) o : output c
            }
    | otherwise =
        error "Unknown CIO"

runMachine' :: Computer -> (Int, Output)
runMachine' c =
    if
        index (memory c) (ip c) == 99
    then
        let
            zeroth = index (memory c) 0
            output' = output c
        in
            (zeroth, output')
    else
        runMachine' $ runInstruction c


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
        runMachine' c
