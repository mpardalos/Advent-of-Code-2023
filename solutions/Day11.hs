{-# LANGUAGE LambdaCase #-}

module Day11 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int64)
import Data.Set (Set)
import Data.Set qualified as Set
import Util (manhattanDistance, uniquePairs)

type Position = (Int64, Int64)

type Size = (Int64, Int64)

part1 :: ByteString -> Int64
part1 = sum . uncurry (getGalaxyDistances 2) . parse

part2 :: ByteString -> Int64
part2 = sum . uncurry (getGalaxyDistances 1000000) . parse

getGalaxyDistances ::
  Int64 ->
  Size ->
  Set Position ->
  [Int64]
getGalaxyDistances expansionMultiplier (height, width) galaxies =
  let emptyRows =
        [ row
          | row <- [0 .. fromIntegral height - 1],
            all
              (`Set.notMember` galaxies)
              [ (row, col)
                | col <- [0 .. fromIntegral width - 1]
              ]
        ]
      emptyColumns =
        [ col
          | col <- [0 .. fromIntegral height - 1],
            all
              (`Set.notMember` galaxies)
              [ (row, col)
                | row <- [0 .. fromIntegral width - 1]
              ]
        ]

      shiftGalaxy (row, col) =
        let rowShift = (expansionMultiplier - 1) * fromIntegral (length (filter (< row) emptyRows))
            colShift = (expansionMultiplier - 1) * fromIntegral (length (filter (< col) emptyColumns))
         in (row + rowShift, col + colShift)

      shiftedGalaxies = Set.map shiftGalaxy galaxies
   in map (uncurry manhattanDistance) (uniquePairs (Set.toList shiftedGalaxies))

-- | ((rows of input, columns of input), galaxy positions)
parse :: ByteString -> (Size, Set Position)
parse input =
  ( ( fromIntegral $ BS.length (head (BS.lines input)),
      fromIntegral $ length (BS.lines input)
    ),
    fst (BS.foldl' insertChar (Set.empty, (0, 0)) input)
  )
  where
    -- insertChar :: (Set Position, Position) -> Char -> (Set Position, Position)
    insertChar (acc, (row, col)) = \case
      '.' -> (acc, (row, col + 1))
      '#' -> (Set.insert (row, col) acc, (row, col + 1))
      '\n' -> (acc, (row + 1, 0))
      _ -> error "Invalid char"
