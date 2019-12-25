import Data.Map.Lazy (Map)
import IntCode (Computer(..), Status (..), runInstruction)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Lazy as Map
import Debug.Trace (trace)

type ShipSide = Map Point Int

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Eq, Show, Ord)
data Robot = Robot {rx :: Int, ry :: Int, rd :: Direction} deriving (Show)

getRawInput :: IO String
getRawInput = readFile "./day11.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=',') s
          rest = drop (length firstword + 1) s

robotLocation :: Robot -> Point
robotLocation r = Point {x = rx r, y = ry r}

updateRobot :: Robot -> Int -> Robot
updateRobot r i =
    if i == 0 -- left
    then
        case rd r of
            UP -> r {rx = rx r - 1, rd = LEFT}
            DOWN -> r {rx = rx r + 1, rd = RIGHT}
            LEFT -> r {ry = ry r - 1, rd = DOWN}
            RIGHT -> r {ry = ry r + 1, rd = UP}
    else -- right
        case rd r of
            UP -> r {rx = rx r + 1, rd = RIGHT}
            DOWN -> r {rx = rx r - 1, rd = LEFT}
            LEFT -> r {ry = ry r + 1, rd = UP}
            RIGHT -> r {ry = ry r - 1, rd = DOWN}

paintShip :: Int -> Computer -> Robot -> ShipSide -> ShipSide
paintShip steps c r ship
    | steps == 0 = ship
    | otherwise =
    case status c of
        HALTED -> ship
        RUNNING -> paintShip (steps - 1) (runInstruction c) r ship
        READING ->
            -- trace (show r)
            -- trace ("i: " ++ show (Map.findWithDefault 0 (robotLocation r) ship))
            paintShip (steps - 1) (
                runInstruction c {
                    input = Map.findWithDefault 0 (robotLocation r) ship : input c
                })
            r ship
        WRITING ->
            case length $ output c of
                1 ->
                    paintShip (steps - 1) (runInstruction c) r ship
                2 ->
                    paintShip
                        (steps - 1)
                        (runInstruction c { output = [] })
                        (updateRobot r (head $ output c))
                        (Map.insert (robotLocation r) (head $ tail $ output c) ship)
                ol -> error $ "Weird output length: " ++ show ol

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
    let shipSide = Map.fromList [] :: ShipSide
    let robot = Robot 0 0 UP
    let result = paintShip 2000000 computer robot shipSide
    print $ Map.size result
