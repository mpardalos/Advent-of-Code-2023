{-# LANGUAGE OverloadedStrings #-}

module Day3 (part1, part2) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.Maybe (catMaybes, isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V

-- | (row, column)
type Coords = (Int, Int)

type Grid = Vector ByteString

part1 :: ByteString -> Int
part1 input =
  let grid = parse input
      partNums =
        catMaybes $
          [ partNumAt grid (row, column)
            | row <- [0 .. length grid - 1],
              column <- [0 .. BS.length (V.head grid) - 1]
          ]
   in sum partNums

partNumAt :: Grid -> Coords -> Maybe Int
partNumAt grid (row, column) = do
  let numStr = BS.takeWhile isDigit $ BS.drop column (grid V.! row)
  (num, _) <- BS.readInt numStr
  let numCoords = [(row, column + dcolumn) | dcolumn <- [0 .. BS.length numStr - 1]]
      neighbourCoords = concatMap neighbours numCoords
  guard (any hasSymbol neighbourCoords)
  -- Fail if we are parsing the middle of a number.
  -- This is to avoid double-counting
  guard (not (isInNum (row, column - 1)))
  return num
  where
    isInNum :: Coords -> Bool
    isInNum coords = isJust $ do
      char <- gridLookup grid coords
      guard (isDigit char)

    hasSymbol :: Coords -> Bool
    hasSymbol coords = isJust $ do
      char <- gridLookup grid coords
      guard (not (isDigit char || char == '.'))

gridLookup :: Grid -> Coords -> Maybe Char
gridLookup grid (rowIdx, columnIdx) = do
  row <- grid V.!? rowIdx
  row BS.!? columnIdx

neighbours :: Coords -> [Coords]
neighbours (row, column) =
  [ (row + drow, column + dcolumn)
    | drow <- [-1, 0, 1],
      dcolumn <- [-1, 0, 1],
      not (dcolumn == 0 && drow == 0)
  ]

part2 :: ByteString -> Int
part2 input = 0

parse :: ByteString -> Grid
parse = V.fromList . BS.lines
