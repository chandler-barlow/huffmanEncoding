module Tree
  ( fromLevelOrder,
    toLevelOrder,
    listToLevelOrder,
    Tree (..),
    flatLevelOrder,
    singleton,
  )
where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq)

instance (Ord a) => Ord (Tree a) where
  (<=) Empty Empty = True
  (<=) (Node a _ _) (Node b _ _) = a <= b
  (<=) Empty _ = True
  (<=) _ Empty = False

singleton :: a -> Tree a
singleton a = Node a Empty Empty

rootVal :: Tree a -> Maybe a
rootVal (Node a _ _) = Just a
rootVal Empty = Nothing

-- drops head and divides level order representation
-- into two level order representation trees
splitLevelOrder :: [[a]] -> ([[a]], [[a]])
splitLevelOrder levels = foldr splitAndAccumulate ([], []) numberedLevels
  where
    splitAndAccumulate numberedLevel acc = accumulate acc (splitLevel numberedLevel)
    accumulate (rs, ls) (r, l) = (r : rs, l : ls)
    -- split a level into two lower levels
    splitLevel (n, level) = let n' = 2 ^ (n - 1) in splitAt n' level
    -- level order rep without head
    levels' = tail levels
    -- tuples of the target level with the representation that needs to be split
    numberedLevels = zip [1 .. length levels'] levels'

fromLevelOrder :: [[a]] -> Tree a
fromLevelOrder [] = Empty
fromLevelOrder xs = Node rootVal (fromLevelOrder l) (fromLevelOrder r)
  where
    (l, r) = splitLevelOrder xs
    rootVal = head . head $ xs

_toLevelOrder :: [Tree a] -> [[a]] -> [[a]]
_toLevelOrder queue res = case nextRes of
  [] -> reverse res
  _ -> _toLevelOrder nextQueue (nextRes : res)
  where
    processNode node (queue', res') = case node of
      Node val right left -> (right : left : queue', val : res')
      Empty -> (queue', res')
    (nextQueue, nextRes) = foldr processNode ([], []) queue

toLevelOrder :: Tree a -> [[a]]
toLevelOrder (Node val right left) = _toLevelOrder [right, left] [[val]]
toLevelOrder Empty = []

_listToLevelOrder :: [a] -> Int -> [[a]]
_listToLevelOrder [] _ = []
_listToLevelOrder l n = take n l : _listToLevelOrder (drop n l) (2 ^ n)

listToLevelOrder :: [a] -> [[a]]
listToLevelOrder l = _listToLevelOrder l 1

flatLevelOrder :: Tree a -> [a]
flatLevelOrder = concat . toLevelOrder

instance (Show a) => Show (Tree a) where
  show Empty = "Empty"
  show (Node hn r l) = concat [show hn, " (", show r, ") ", "(", show l, ")"]
