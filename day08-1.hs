import Data.Char (isDigit)
import Data.List (elemIndex)
import Debug.Trace (trace)

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

numberOfDigits :: Int -> Layer -> Int
numberOfDigits d layer =
    let
        filtered = map (filter (== d)) layer :: Layer
        counted = map length filtered :: [Int]
    in sum counted

main :: IO ()
main = do
    rawInput <- getRawInput
    let layers = splitLayers 25 6 rawInput
    let numZeros = map (numberOfDigits 0) layers
    let maxZeros = minimum numZeros
    let total = case elemIndex maxZeros numZeros of
                    Nothing -> error "no max zeros layer?"
                    Just i -> product [numberOfDigits 1 (layers !! i), numberOfDigits 2 (layers !! i)]
    print total
