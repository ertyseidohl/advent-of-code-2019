getRawInput :: IO String
getRawInput = readFile "./day06.input"

split :: String -> [String]
split "" = []
split s = firstword : split rest
    where firstword = takeWhile (/=')') s
          rest = drop (length firstword + 1) s

tuplize :: [String] -> (String, String)
tuplize (x:y:_) = (x, y)
tuplize xs = error $ "Error in tuplize" ++ show xs

data Tree = Empty | Node {str :: String, children :: [Tree]} deriving (Show, Eq)

toEmptyNode :: [(String, String)] -> (String, String) -> Tree
toEmptyNode tuples (_, c) = toTree (Node c []) tuples

findChildren:: String -> [(String, String)] -> [Tree]
findChildren search tuples =
    let found = filter (\t -> fst t == search) tuples
    in map (toEmptyNode tuples) found

toTree :: Tree -> [(String, String)] -> Tree
toTree Empty _ = error "Empty tree?"
toTree root tuples = Node (str root) $ findChildren (str root) tuples

count :: Tree -> Int
count node = 0 + sum (map (count' 1) (children node))

count' :: Int -> Tree -> Int
count' depth node = depth + sum (map (count' (depth + 1)) (children node))

main :: IO ()
main = do
    input <- getRawInput
    let linesInput = lines input
    let splitInput = map split linesInput
    let tupleInput = map tuplize splitInput
    let tree = toTree (Node "COM" []) tupleInput
    print $ count tree

