module Lib where

-- An attempt at huffman encoding
import Data.List (nub, sort, sortBy)
import qualified Tree as T

-- this just makes an association list of frequencies
makeFreqTable :: Eq a => [a] -> [(a, Int)]
makeFreqTable xs = sortFreqTable $ zip uniqXs countUniq
  where
    uniqXs = nub xs
    countUniq = map (`count` xs) uniqXs
    count t = sum . map (\x -> if x == t then 1 else 0)
    sortFreqTable = sortBy (\(_, n1) (_, n2) -> if n1 < n2 then GT else LT)

showTable :: Show a => [(a, Int)] -> String
showTable = foldl (\acc entry -> acc ++ showEntry entry) "Frequency Table: \n"
  where
    showEntry (val, count) = concat [show val, " occurs ", show count, " times\n"]

trav :: Int -> [String]
trav n = take n $ cycle ["0", "1"]

toTrav :: [[a]] -> [[String]]
toTrav = map (trav . length)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)

_travToCodes :: [String] -> [[String]] -> [[String]]
_travToCodes _ [] = []
_travToCodes p (t : ts) = thisLevel : _travToCodes thisLevel ts
  where
    thisLevel :: [String]
    thisLevel = concat $ zipWith map (map (++) p) (chunk 2 t)

travToCodes :: [[String]] -> [[String]]
travToCodes ts = head ts : _travToCodes (head ts) (tail ts)

huffmanTable :: Eq a => [a] -> [(a, String)]
huffmanTable l = zipWith (\row code -> (fst row, code)) ftable codes
  where
    ftable = makeFreqTable l
    createCodes = concat . travToCodes . toTrav . T.listToLevelOrder
    codes = createCodes ftable

newtype FakeHeap a = FakeHeap [a]

insert :: Ord a => a -> FakeHeap a -> FakeHeap a
insert v (FakeHeap l) = FakeHeap . sort $ v : l

pop :: FakeHeap a -> (a, FakeHeap a)
pop (FakeHeap l) = (head l, FakeHeap (tail l))

main = print "hello world"
