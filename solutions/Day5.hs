{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (foldl', unfoldr)
import Data.Maybe (isJust)
import Safe (fromJustNote)

type CategoryMap = [(Int64, Int64, Int64)]

part1 :: ByteString -> Int64
part1 input =
  let (seeds, categoryMaps) = parse input
      applyAllMaps = foldl' (\f categoryMap -> applyMap categoryMap . f) id categoryMaps
   in minimum (map applyAllMaps seeds)

part2 :: ByteString -> Int
part2 _ = -1

applyMap :: CategoryMap -> Int64 -> Int64
applyMap ((dstStart, srcStart, range) : mapEntries) srcNum =
  if srcStart <= srcNum && srcNum < srcStart + range
    then dstStart + srcNum - srcStart
    else applyMap mapEntries srcNum
applyMap [] srcNum = srcNum

parse :: ByteString -> ([Int64], [CategoryMap])
parse input =
  let inputLines = BS.lines input
      seeds :: [Int64] =
        head inputLines
          & BS.drop 7
          & unfoldr (BS.readInt . BS.dropSpace)
          & map fromIntegral
   in (seeds, unfoldr readMap (drop 1 inputLines))
  where
    readMapLine :: ByteString -> Maybe (Int64, Int64, Int64)
    readMapLine (unfoldr (BS.readInt . BS.dropSpace) -> [a, b, c]) =
      Just (fromIntegral a, fromIntegral b, fromIntegral c)
    readMapLine _ = Nothing

    readMap :: [ByteString] -> Maybe (CategoryMap, [ByteString])
    readMap (_emptyline : _label : inputLines) =
      let mapLines = takeWhile (isJust . readMapLine) inputLines
          restLines = dropWhile (isJust . readMapLine) inputLines
       in Just (map (fromJustNote "Could not read map line" . readMapLine) mapLines, restLines)
    readMap _ = Nothing
