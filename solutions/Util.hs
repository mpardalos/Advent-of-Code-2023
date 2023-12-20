{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Util where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace)

parseOrError :: Parser a -> ByteString -> a
parseOrError parser input = case parseOnly parser input of
  Left e -> error ("Parse error: " <> show e)
  Right v -> v

hashNub :: Hashable a => [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

readIntsSepBy :: Char -> ByteString -> [Int]
readIntsSepBy c = unfoldr (BS.readInt . (BS.dropWhile (== c)))

readSpacedInts :: ByteString -> [Int]
readSpacedInts = unfoldr (BS.readInt . BS.dropSpace)

pairwise :: [a] -> [(a, a)]
pairwise (a : b : rest) = (a, b) : pairwise (b : rest)
pairwise _ = []

-- | Get all unordered pairs of elements from a list.
-- The cartesian product of the list with itself
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (row1, col1) (row2, col2) = abs (row2 - row1) + abs (col2 - col1)

traceShowIdLabelled :: Show a => String -> a -> a
traceShowIdLabelled l x = trace (l ++ ": " ++ show x) x

type Position = (Int, Int)

newtype SparseGrid a = SparseGrid (Map Position a)
  deriving newtype (Functor, Foldable)

newtype DenseGrid a = DenseGrid (Vector (Vector a))
  deriving (Functor, Foldable)

class Grid g where
  type LookupType g x

  size :: g a -> (Int, Int)

  set :: Position -> (LookupType g a) -> g a -> g a
  set p x g = update p (const x) g

  get :: Position -> g a -> LookupType g a

  update :: Position -> (LookupType g a -> LookupType g a) -> g a -> g a

  getRow :: Int -> g a -> [LookupType g a]
  getColumn :: Int -> g a -> [LookupType g a]

instance Grid SparseGrid where
  type LookupType SparseGrid x = Maybe x

  size (SparseGrid m) =
    ( maximum (map fst (Map.keys m)) + 1,
      maximum (map snd (Map.keys m)) + 1
    )

  set p (Just x) (SparseGrid m) = SparseGrid (Map.insert p x m)
  set p Nothing (SparseGrid m) = SparseGrid (Map.delete p m)

  get p = Map.lookup p . coerce
  update p f (SparseGrid m) = SparseGrid (Map.alter f p m)

  getRow row g@(SparseGrid m) =
    [ Map.lookup (row, col) m
      | col <- [0 .. snd (size g) - 1]
    ]
  getColumn col g@(SparseGrid m) =
    [ Map.lookup (row, col) m
      | row <- [0 .. fst (size g) - 1]
    ]

instance Grid DenseGrid where
  type LookupType DenseGrid x = x

  size (DenseGrid v) = (length v, length (V.head v))

  update (targetRow, targetCol) f =
    coerce $ V.imap $ \row ->
      V.imap $ \col x ->
        if (row, col) == (targetRow, targetCol)
          then f x
          else x

  get (row, col) (DenseGrid v) = v V.! row V.! col

  getRow row (DenseGrid v) = V.toList (v V.! row)
  getColumn col (DenseGrid v) = map (V.! col) (V.toList v)

inBounds :: Grid g => g a -> Position -> Bool
inBounds g (row, col) =
  let (height, width) = size g
   in row >= 0 && col >= 0 && row < height && col < width

parseDenseGrid :: ByteString -> DenseGrid Char
parseDenseGrid input =
  DenseGrid $
    V.fromList $
      map (V.fromList . BS.unpack) $
        BS.lines input

parseSparseGrid :: (Char -> Bool) -> ByteString -> SparseGrid Char
parseSparseGrid isEmpty input =
  SparseGrid $
    go Map.empty (0, 0) (BS.unpack input)
  where
    go m _ [] = m
    go m (row, col) (c : cs) = case c of
      '\n' -> go m (row + 1, 0) cs
      _ | isEmpty c -> go m (row, col + 1) cs
      _ -> go (Map.insert (row, col) c m) (row, col + 1) cs
