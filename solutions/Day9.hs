{-# LANGUAGE OverloadedStrings #-}

module Day9 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Data.Function ((&))
import Util (pairwise, readSpacedInts)

type Sequence = [Int]

part1 :: ByteString -> Int
part1 input =
  parse input
    & map predict
    & sum

predict :: Sequence -> Int
predict topSequence =
  foldl'
    (\prediction s -> last s + prediction)
    0
    (differentialChain topSequence)

part2 :: ByteString -> Int
part2 input =
  parse input
    & map backPredict
    & sum

backPredict :: Sequence -> Int
backPredict topSequence =
  foldr
    (\s backPrediction -> head s - backPrediction)
    0
    (differentialChain topSequence)

differentialChain :: Sequence -> [Sequence]
differentialChain = takeWhile (any (/= 0)) . iterate differentiate

differentiate :: Sequence -> Sequence
differentiate = map (uncurry (flip (-))) . pairwise

parse :: ByteString -> [Sequence]
parse input = map readSpacedInts $ BS.lines input
