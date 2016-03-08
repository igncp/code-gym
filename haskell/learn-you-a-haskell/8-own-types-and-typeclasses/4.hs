data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

str :: String
str = "foo bar"

charsTree :: Tree Char
charsTree = foldr treeInsert EmptyTree str

main :: IO()
main = do
  putStrLn $ "charsTree from '" ++ str ++ "'"
  putStrLn $ "charsTree: " ++ show charsTree
  putStrLn $ "treeElem o: " ++ (show $ treeElem 'o' charsTree)
  putStrLn $ "treeElem z: " ++ (show $ treeElem 'z' charsTree)
