{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Day16 (part1, part2) where

import Control.Applicative (Applicative (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Set qualified as Set
import Data.SparseGrid
import Debug.Trace (trace, traceShow)

pattern East, South, West, North :: (Eq a1, Eq a2, Num a1, Num a2) => (a1, a2)
pattern East = (0, 1)
pattern South = (1, 0)
pattern West = (0, -1)
pattern North = (-1, 0)

addPos (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

part1 :: ByteString -> Int
part1 bs =
  parse bs
    & getEnergized
    & rows
    & concat
    & filter id
    & length

getEnergized :: SparseGrid Char -> SparseGrid Bool
getEnergized grid = snd $ go Set.empty (False <$ grid) (0, 0) East
  where
    go visited energizedGrid pos dir@(rowD, colD) =
      let energizedGrid' = set pos True energizedGrid
          visited' = Set.insert (pos, dir) visited
          keepGoing v = go v energizedGrid' (addPos pos dir) dir
          goNorth v = go v energizedGrid' (addPos pos North) North
          goWest v = go v energizedGrid' (addPos pos West) West
          goSouth v = go v energizedGrid' (addPos pos South) South
          goEast v = go v energizedGrid' (addPos pos East) East
       in case get pos grid of
            _ | not (inBounds grid pos) -> (visited, energizedGrid)
            _ | (pos, dir) `elem` visited -> (visited, energizedGrid)
            '.' -> keepGoing visited'
            '/' -> case dir of
              East -> goNorth visited'
              South -> goWest visited'
              West -> goSouth visited'
              North -> goEast visited'
            '\\' -> case dir of
              East -> goSouth visited'
              South -> goEast visited'
              West -> goNorth visited'
              North -> goWest visited'
            '-'
              | colD == 0 ->
                  let (visited'', g1) = goEast visited'
                      (visited''', g2) = goWest visited''
                   in (visited''', liftA2 (||) g1 g2)
              | otherwise -> keepGoing visited'
            '|'
              | rowD == 0 ->
                  let (visited'', g1) = goNorth visited'
                      (visited''', g2) = goSouth visited''
                   in (visited''', liftA2 (||) g1 g2)
              | otherwise -> keepGoing visited'

part2 :: ByteString -> ()
part2 _ = ()

parse :: ByteString -> SparseGrid Char
parse = parseSparseGrid '.'

displayEnergized :: SparseGrid Bool -> String
displayEnergized = displayGrid . fmap (\x -> if x then '#' else '.')

example1Test :: IO ()
example1Test =
  parse example1
    & getEnergized
    & displayEnergized
    & putStrLn

example1 :: ByteString
example1 =
  BS.unlines
    [ ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|...."
    ]
