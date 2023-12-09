{-# LANGUAGE OverloadedStrings #-}

module Day9 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Util (pairs, readSpacedInts)

type Sequence = [Int]

part1 :: ByteString -> Int
part1 input =
  parse input
    & map predict
    & sum

predict :: Sequence -> Int
predict topSequence = go (differentialChain topSequence)
  where
    go [] = 0
    go [s] = head s -- Last sequence in the chain, should be all equal, so no need to traverse to the end.
    go (s : diffs) = last s + go diffs

part2 :: ByteString -> Int
part2 input =
  parse input
    & map backPredict
    & sum

backPredict :: Sequence -> Int
backPredict topSequence = go (differentialChain topSequence)
  where
    go [] = 0
    go [s] = head s
    go (s : diffs) = head s - go diffs

differentialChain :: Sequence -> [Sequence]
differentialChain = takeWhile (any (/= 0)) . iterate differentiate

differentiate :: Sequence -> Sequence
differentiate = map (uncurry (flip (-))) . pairs

parse :: ByteString -> [Sequence]
parse input = map readSpacedInts $ BS.lines input
