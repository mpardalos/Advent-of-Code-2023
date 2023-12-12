{-# LANGUAGE OverloadedStrings #-}

module Day12 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
-- import Debug.Trace (trace)
import Text.Printf (printf)
import Util (readIntsSepBy)
import Data.List (intercalate)

trace :: String -> a -> a
trace _ x = x

part1 :: ByteString -> Int
part1 = sum . map (uncurry countConfigurations) . parse

countConfigurations :: [Char] -> [Int] -> Int
countConfigurations = go False
  where
    go :: Bool -> [Char] -> [Int] -> Int
    go inContiguous (char : chars) (count : counts) = case (count, char) of
      (_, _) | trace (printf "inContiguous=%-5s | char=%c | count=%d | %s" (show inContiguous) char count (char : chars)) False -> undefined
      (0, '#') -> trace "end=0 (too many #)" 0
      (0, '.') -> go False chars counts
      (0, '?') -> trace "is ." (go False chars counts)
      (_, '.') -> if inContiguous then 0 else go False chars (count : counts)
      (_, '#') -> go True chars (count - 1 : counts)
      (_, '?') ->
        if inContiguous
          then trace "is #" (go True chars (count - 1 : counts))
          else let
                asDot = go False chars (count : counts)
                asHash = go True chars (count - 1 : counts)
      (_, _) -> error ("Invalid character: " ++ show char)
    go _ [] [0] = trace "end=1 (end of input)" 1
    go _ [] (_ : _) = trace "end=0 (end of input)" 0
    go inContiguous chars [] =
      if not inContiguous && not (any (== '#') chars)
        then trace "end=1" 1
        else trace "end=0" 0

part2 :: ByteString -> ()
part2 _ = ()

part2ify :: ([Char], [Int]) -> ([Char], [Int])
part2ify (chars, counts) = (intercalate "?" (replicate 5 chars), concat (replicate 5 counts))

parse :: ByteString -> [([Char], [Int])]
parse input =
  BS.lines input
    & map
      ( \line ->
          let [recordsStr, numStr] = BS.splitWith (== ' ') line
           in (BS.unpack recordsStr, readIntsSepBy ',' numStr)
      )

example1 :: [([Char], [Int])]
example1 =
  parse . BS.unlines $
    [ "#.#.### 1,1,3",
      ".#...#....###. 1,1,3",
      ".#.###.#.###### 1,3,1,6",
      "####.#...#... 4,1,1",
      "#....######..#####. 1,6,5",
      ".###.##....# 3,2,1"
    ]

example1p2 = map part2ify example1

example2 :: [([Char], [Int])]
example2 =
  parse . BS.unlines $
    [ "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1"
    ]

example2p2 = map part2ify example2
