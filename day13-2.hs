import Debug.Trace (trace)
import IntCode (Computer(..), Status (..), err, runInstruction)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
-- import System.IO (stdin, stdout, hSetEcho, hReady, hSetBuffering, BufferMode(NoBuffering))

type GameScreen = Map (Int, Int) Int

getRawInput :: IO String
getRawInput = readFile "./day13-modified.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

-- from https://stackoverflow.com/a/38553473/374601
-- getKey :: IO String
-- getKey = reverse <$> getKey' ""
--   where getKey' chars = do
--           char <- getChar
--           more <- hReady stdin
--           (if more then getKey' else return) (char:chars)

updateScreen :: GameScreen -> [Int] -> GameScreen
updateScreen screen [] = screen
updateScreen screen (x:y:t:rest) = updateScreen (Map.insert (x, y) t screen) rest
updateScreen _ e = error $ "Non multiple of 3 in updateScreen" ++ show e

runUntilBlock :: Computer -> Computer
runUntilBlock c
    | status c == HALTED = c
    | status c == READING = c
    | status c == WRITING = runUntilBlock $ runInstruction c
    | status c == RUNNING = runUntilBlock $ runInstruction c
    | otherwise = error $ err "Unknown status" c

screenSize :: ((Int, Int) -> Int) -> GameScreen -> Int
screenSize a s = maximum [a c | c <- Map.keys s]

showScreen :: GameScreen -> String
showScreen s = showScreen' s (screenSize fst s) (screenSize snd s) 0 0 ""

toSymbol :: Maybe Int -> Char
toSymbol i =
    case i of
        Nothing -> ' '
        Just 0 -> ' '
        Just 1 -> '#'
        Just 2 -> '@'
        Just 3 -> '_'
        Just 4 -> 'O'
        Just x -> error $ "Unknown tile " ++ show x

showScreen' :: GameScreen -> Int -> Int -> Int -> Int -> String -> String
showScreen' s w h x y buf
    | x == w && y == h = '\n' : buf
    | x == w = showScreen' s w h 0 (y + 1) ('\n' : buf)
    | otherwise = showScreen' s w h (x + 1) y (toSymbol (Map.lookup (x, y) s) : buf)

gameLoop :: Computer -> GameScreen -> IO ()
gameLoop c screen = do
    -- hSetBuffering stdin NoBuffering
    -- hSetBuffering stdout NoBuffering
    -- hSetEcho stdin False
    -- key <- getKey
    -- let keybuf = case key of
    --                 "\ESC[C" -> [-1] -- Right
    --                 "\ESC[D" -> [1] -- Left
    --                 _ -> [0]
    let c' = runUntilBlock c
    let screen' = updateScreen screen (reverse $ output c')
    putStr $ showScreen screen'
    putStr $ case Map.lookup (-1,0) screen' of
        Nothing -> "Score: X"
        Just x -> "Score: " ++ show x
    let c'' = runInstruction c' {output = [], input=[0]}
    if status c'' /= HALTED
    then gameLoop c'' screen'
    else print "done"

main :: IO ()
main = do
    rawInput <- getRawInput
    let splitInput = split rawInput
    let intCode = map read splitInput :: [Int]
    let computer = Computer {
        memory = IntMap.fromList $ zip [0..] intCode
        , input = []
        , output = []
        , ip = 0
        , status = RUNNING
        , compId = 0
        , relativeBase = 0
    }
    gameLoop computer (Map.empty :: GameScreen)
