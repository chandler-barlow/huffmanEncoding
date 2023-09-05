module Ftable where

import qualified Data.Bifunctor as BF
import qualified Data.ByteString as B
import Data.Function (on)
import Data.List (nub, sortBy)
import qualified Data.Word as W
import qualified Tree as T

data HuffNode a = HuffNode a Int | HuffInternal Int

type HuffTree a = T.Tree (HuffNode a)

instance (Show a) => Show (HuffNode a) where
  show (HuffNode s c) = concat ["Node ", show s, " count: ", show c]
  show (HuffInternal c) = "Internal count: " ++ show c

getRootCount :: HuffTree a -> Int
getRootCount (T.Node (HuffNode _ c) _ _) = c
getRootCount (T.Node (HuffInternal c) _ _) = c
getRootCount T.Empty = 0

getSymbol :: HuffNode a -> Maybe a
getSymbol (HuffNode s _) = Just s
getSymbol _ = Nothing

count :: (Eq a) => a -> [a] -> Int
count e = sum . map (\x -> if e == x then 1 else 0)

frequency :: (Eq a) => a -> [a] -> (a, Int)
frequency e xs = (e, count e xs)

frequencyTable :: (Eq a) => [a] -> [(a, Int)]
frequencyTable xs = map (`frequency` xs) uniques
  where
    uniques = nub xs

sort :: [HuffTree a] -> [HuffTree a]
sort = sortBy (compare `on` getRootCount)

insert :: HuffTree a -> [HuffTree a] -> [HuffTree a]
insert n xs = sort xs'
  where
    xs' = n : xs

huffTreeFromHuffList :: [HuffTree a] -> HuffTree a
huffTreeFromHuffList [] = T.Empty
huffTreeFromHuffList [n] = n
huffTreeFromHuffList (n1 : n2 : ns) = huffTreeFromHuffList ns'
  where
    root = T.Node (HuffInternal combinedRootCount) n1 n2
    combinedRootCount = getRootCount n1 + getRootCount n2
    ns' = insert root ns

_buildTravTree :: Encoding -> T.Tree a -> T.Tree Encoding
_buildTravTree n (T.Node _ l r) = T.Node n (_buildTravTree [False] l) (_buildTravTree [True] r)
_buildTravTree _ _ = T.Empty

buildTravTree :: T.Tree a -> T.Tree [Bool]
buildTravTree = _buildTravTree [False]

walkTree :: T.Tree Encoding -> T.Tree Encoding
walkTree = _walkTree [False]
  where
    _walkTree encoding (T.Node val left right) =
      let encoding' = encoding ++ val
          left' = _walkTree encoding' left
          right' = _walkTree encoding' right
       in T.Node encoding' left' right'
    _walkTree _ _ = T.Empty

isInternal :: HuffNode a -> Bool
isInternal HuffInternal {} = True
isInternal HuffNode {} = False

createHuffTree :: (Ord a) => [a] -> HuffTree a
createHuffTree = huffTreeFromHuffList . buildHuffList
  where
    buildHuffList = map (T.singleton . uncurry HuffNode) . frequencyTable

encodeTree :: T.Tree a -> T.Tree [Bool]
encodeTree = walkTree . buildTravTree

-- This might be a better function than the other one for removing internal nodes
removeInternalNodes :: [(HuffNode a, Encoding)] -> HuffTable a
removeInternalNodes = foldl onlyKeepNodes []
  where
    onlyKeepNodes nodes' (node, encoding) =
      case getSymbol node of
        Just symbol -> (symbol, encoding) : nodes'
        _ -> nodes'

type HuffTable a = [(a, Encoding)]

type Encoding = [Bool]

buildHuffTable :: (Ord a) => [a] -> HuffTable a
buildHuffTable xs = removeInternalNodes pairs
  where
    huffTree = createHuffTree xs
    encodedTree = encodeTree huffTree
    pairs = zip (T.flatLevelOrder huffTree) (T.flatLevelOrder encodedTree)

-- Seems kind of messy idk
-- could probably be cleaned up lmao
encode :: (Ord a) => [a] -> (HuffTable a, Encoding)
encode s = (huffTable, encoded)
  where
    huffTable = buildHuffTable s
    encoded = concatMap encodeChar s
    encodeChar c =
      let entry = lookup c huffTable
       in case entry of
            Just code -> code
            _ -> []

decode = "todo"

toBinaryString :: [Bool] -> String
toBinaryString = concatMap (\bit -> if bit then "1" else "0")

byteToWord8 :: [Bool] -> W.Word8
byteToWord8 byte = sum $ zipWith (*) digits bits
  where
    bits = map (\b -> if b then 1 else 0) byte
    digits = [2 ^ n | n <- [0 .. 8]]

toByteString :: [Bool] -> B.ByteString
toByteString bits = B.pack $ map byteToWord8 asBits
  where
    asBits = chunk 8 bits

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

asInt :: [Bool] -> Int
asInt bits = sum $ zipWith (*) digits ns
  where
    digits = [2 ^ n | n <- [0 .. length bits]]
    ns = map (\e -> if e then 1 else 0) bits

formatTable :: HuffTable Char -> String
formatTable t = concatMap showEntry t ++ "~"
  where
    showEntry (symbol, encoding) = symbol : concat [" ", show $ asInt encoding, " "]
