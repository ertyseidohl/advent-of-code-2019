module IntCode (runMachine) where

import Debug.Trace (trace)
import Data.Sequence (Seq(..), index, insertAt, update, (!?))
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

err :: String -> Computer -> String
err s c = s ++ " IP: " ++ show (ip c) ++ " mem: " ++ show (memory c)

runInstruction :: Computer -> Computer
runInstruction c =
    case memory c !? ip c of
        Nothing -> error $ err "Bad instruction lookup." c
        Just i -> case i of
            1 -> math (+) c
            2 -> math (*) c
            3 -> cio INPUT c
            4 -> cio OUTPUT c
            _ -> error $ err "Bad instruction." c

math :: (Int -> Int -> Int) -> Computer -> Computer
math op c
    | length (memory c) < ip c + 3 = error $ err "Out of range math." c
    | otherwise =
        let
            ipc = ip c
            x = index (memory c) (ipc + 1)
            y = index (memory c) (ipc + 2)
            o = index (memory c) (ipc + 3)
        in
            let
                xv = index (memory c) x
                yv = index (memory c) y
            in
                c {
                    memory = update o (xv `op` yv) (memory c),
                    ip = ipc + 4
                }

cio :: IOType -> Computer -> Computer
cio i c =
    case i of
        INPUT -> case memory c !? (ip c + 1) of
            Nothing -> error $ err "Bad INPUT instruction." c
            Just il -> c {
                memory = insertAt il (head $ input c) (memory c),
                ip = ip c + 2,
                input = tail $ input c
            }
        OUTPUT -> case memory c !? (ip c + 1) of
            Nothing -> error $ err "Bad OUTPUT instruction." c
            Just ol -> c {
                ip = ip c + 2,
                output = case memory c !? ol of
                    Nothing -> error $ err "Bad OUTPUT origin." c
                    Just x -> x : output c
            }

runMachine' :: Computer -> (Int, Output)
runMachine' c =
    case memory c !? ip c of
        Nothing -> error $ err "Bad instruction index." c
        Just 99 ->
            let
                zeroth = case memory c !? 0 of
                    Nothing -> error $ err "Empty memory." c
                    Just x -> x
                output' = output c
            in
                (zeroth, output')
        Just _ -> runMachine' $ runInstruction c


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
