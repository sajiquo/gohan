module QuickSort
    ( quickSort
    ) where

data Tree =  Empty | Node {
    value :: Int,
    left  :: Tree,
    right :: Tree
} deriving Show

-- insert integer to Node
insert :: Tree -> Int -> Tree
insert Empty x = Node { value = x, left = Empty, right = Empty}
insert (Node { value = y, left = aLeft, right = aRight}) x
  | x < y = Node y (insert aLeft x) aRight
  | otherwise = Node y aLeft (insert aRight x)

-- convert list of integer to Node
buildTree :: [Int] -> Tree
buildTree = foldl insert Empty

traverseTree :: Tree -> [Int]
traverseTree Node{ value=x, left=l, right=r} = traverseTree l ++ [x] ++ traverseTree r
traverseTree Empty = []

quickSort :: [Int] -> [Int]
quickSort = traverseTree . buildTree -- randomizeまじでめんどくさいので割愛…


