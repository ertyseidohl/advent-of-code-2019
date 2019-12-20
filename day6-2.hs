import Debug.Trace (trace)

getRawInput :: IO String
getRawInput = readFile "./day6.input"

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

pathLen :: String -> Tree -> Maybe Int
pathLen = pathLen' 0

pathLen' :: Int -> String -> Tree -> Maybe Int
pathLen' depth target tree
    | str tree == target = Just (0 + depth) -- don't count end node
    | null $ children tree = Nothing
    -- maximum returns the only Just value from the list (or Nothing if the list is all Nothing)
    | otherwise = maximum $ map (pathLen' (1 + depth) target) (children tree)

findPathLength :: String -> String -> Tree -> Maybe Int
findPathLength t1 t2 tree =
    let
        t1l = pathLen t1 tree
        t2l = pathLen t2 tree
    in
        case (t1l, t2l) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (Just l1, Just l2) -> Just (l1 + l2 - 2) -- subtract starting nodes

maybeMin :: Maybe Int -> Maybe Int -> Maybe Int
maybeMin f s = case (f, s) of
    (Nothing, Nothing) -> Nothing
    (Nothing, b) -> b
    (a, Nothing) -> a
    (a, b) -> if a < b then a else b

minPathLength :: String -> String -> Tree -> Maybe Int
minPathLength t1 t2 tree =
    foldr maybeMin Nothing (
        findPathLength t1 t2 tree : map (minPathLength t1 t2) (children tree)
    )


main :: IO ()
main = do
    input <- getRawInput
    let linesInput = lines input
    let splitInput = map split linesInput
    let tupleInput = map tuplize splitInput
    let tree = toTree (Node "COM" []) tupleInput
    print $ minPathLength "SAN" "YOU" tree

