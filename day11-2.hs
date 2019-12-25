import Data.Map.Lazy (Map)
import IntCode (Computer(..), Status (..), runInstruction)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Lazy as Map
import Debug.Trace (trace)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

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

getBound :: (Int -> Int -> Bool) -> (Point -> Int) -> ShipSide -> Int
getBound minmax prop s =
    foldl (\curr next -> if prop next `minmax` curr then prop next else curr) 0 (Map.keys s)

fillCanvas :: Int -> Int -> Int -> Point -> Int -> Seq Int -> Seq Int
fillCanvas xrange xadj yadj point = Seq.update ((y point + yadj) * xrange + (x point + xadj))

displayResult :: ShipSide -> IO ()
displayResult s =
    let
        minx = getBound (<) x s
        maxx = getBound (>) x s
        miny = getBound (<) y s
        maxy = getBound (>) y s
        xrange = (maxx - minx) + 1
        yrange = (maxy - miny) + 1
        blankCanvas = Seq.replicate (xrange * yrange) 0 :: Seq Int
        xadj = abs $ minimum [0, minx]
        yadj = abs $ minimum [0, miny]
        doneCanvas =
            Map.foldrWithKey (fillCanvas xrange xadj yadj) blankCanvas s :: Seq Int
    in displayResult' xrange (Seq.reverse doneCanvas)

displayResult' :: Int -> Seq Int -> IO ()
displayResult' xrange canvas
    | null canvas = print ""
    | otherwise =
        let
            h = Seq.take xrange canvas :: Seq Int
            l = Seq.drop xrange canvas :: Seq Int
        in do
            print $ (\ g -> if g == 0 then ' ' else '#') <$> Seq.reverse h
            displayResult' xrange l

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
    let shipSide = Map.fromList [(Point 0 0, 1)] :: ShipSide
    let robot = Robot 0 0 UP
    let result = paintShip 2000000 computer robot shipSide
    displayResult result
