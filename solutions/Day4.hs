{-# LANGUAGE OverloadedStrings #-}

module Day4 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.List (unfoldr)
import Data.Set qualified as Set
import Safe (headDef, tailSafe)

data Scratchcard = Scratchcard
  { winning :: ![Int],
    have :: ![Int]
  }

part1 :: ByteString -> Int
part1 = sum . map scoreScratchCard . parse

part2 :: ByteString -> Int
part2 input = go 1 [] (parse input)
  where
    go :: Int -> [Int] -> [Scratchcard] -> Int
    go idx counts (card : cards) =
      let matches = matchCount card
          thisCardCount = 1 + headDef 0 counts
          countsLen = max (length (tailSafe counts)) matches
          counts' =
            take countsLen $
              zipWith
                (+)
                (tailSafe counts ++ repeat 0)
                (replicate matches thisCardCount ++ repeat 0)
       in thisCardCount + go (idx + 1) counts' cards
    go _ _ [] = 0

scoreScratchCard :: Scratchcard -> Int
scoreScratchCard card =
  if matchCount card > 0
    then 2 ^ (matchCount card - 1)
    else 0

matchCount :: Scratchcard -> Int
matchCount Scratchcard {winning, have} =
  length (Set.intersection (Set.fromList winning) (Set.fromList have))

parse :: ByteString -> [Scratchcard]
parse input =
  BS.lines input
    & map
      ( \line ->
          let numsStr = BS.drop 2 (BS.dropWhile (/= ':') line)
              [winningStr, haveStr] = BS.split '|' numsStr
              winning :: [Int] = unfoldr (BS.readInt . BS.dropSpace) winningStr
              have :: [Int] = unfoldr (BS.readInt . BS.dropSpace) haveStr
           in Scratchcard {winning, have}
      )
