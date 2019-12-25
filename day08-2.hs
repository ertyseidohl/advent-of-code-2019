import Data.Char (isDigit)

getRawInput :: IO String
getRawInput = readFile "./day08.input"

type Layer = [[Int]]

splitLayers :: Int -> Int -> String -> [Layer]
splitLayers width height input =
    splitLayers' width height [read [c] :: Int | c <- input, isDigit c]

splitLayers' :: Int -> Int -> [Int] -> [Layer]
splitLayers' width height input
    | null input = []
    | otherwise =
        let
            (curr,next) = splitAt (width * height) input
        in
            toLayer width height curr : splitLayers' width height next

toLayer :: Int -> Int -> [Int] -> Layer
toLayer width height input
    | null input = []
    | otherwise =
        let
            (curr, next) = splitAt width input
        in
            curr : toLayer width height next

combinePixel :: Int -> Int -> Int
combinePixel 2 x = x
combinePixel x _ = x

combineRow :: [Int] -> [Int] -> [Int]
combineRow = zipWith combinePixel

combineLayers :: Layer -> Layer -> Layer
combineLayers = zipWith combineRow

combineImage :: [Layer] -> Layer
combineImage = foldl1 combineLayers

showImage :: Layer -> IO ()
showImage [] = print ""
showImage (r:rs) = do
    print $ map (\x -> if x == 1 then "#" else ".") r
    showImage rs

main :: IO ()
main = do
    rawInput <- getRawInput
    let layers = splitLayers 25 6 rawInput
    print layers
    let image = combineImage layers
    showImage image
