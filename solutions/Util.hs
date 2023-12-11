module Util where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (unfoldr)
import Data.Set qualified as Set
import Debug.Trace (trace)

parseOrError :: Parser a -> ByteString -> a
parseOrError parser input = case parseOnly parser input of
  Left e -> error ("Parse error: " <> show e)
  Right v -> v

hashNub :: Hashable a => [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

readSpacedInts :: ByteString -> [Int]
readSpacedInts = unfoldr (BS.readInt . BS.dropSpace)

pairwise :: [a] -> [(a, a)]
pairwise (a : b : rest) = (a, b) : pairwise (b : rest)
pairwise _ = []

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs

traceShowIdLabelled :: Show a => String -> a -> a
traceShowIdLabelled l x = trace (l ++ ": " ++ show x) x

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (row1, col1) (row2, col2) = abs (row2 - row1) + abs (col2 - col1)
