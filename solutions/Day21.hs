{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day21 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Graph.Inductive ()

import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.SparseGrid (SparseGrid (..))
import Data.SparseGrid qualified as SG
import Optics (over, (.~))
import Safe (headNote, minimumDef)

part1 :: ByteString -> ()
part1 _ = ()

part2 :: ByteString -> ()
part2 _ = ()

data GridTraversal a = GridTraversal
  { location :: SG.Position,
    value :: a,
    north :: Maybe (GridTraversal a),
    east :: Maybe (GridTraversal a),
    south :: Maybe (GridTraversal a),
    west :: Maybe (GridTraversal a)
  }

mapTraversal :: (SG.Position, SparseGrid Char) -> Maybe (GridTraversal Char)
mapTraversal (pos, grid) = gridTraverse (== '.') pos grid

gridTraverse :: Show a => (a -> Bool) -> SG.Position -> SparseGrid a -> Maybe (GridTraversal a)
gridTraverse isOpen (row, col) g =
  let value = SG.get (row, col) g
   in if isOpen value
        then
          Just
            GridTraversal
              { location = (row, col),
                value,
                north = gridTraverse isOpen (row - 1, col) g,
                east = gridTraverse isOpen (row, col + 1) g,
                south = gridTraverse isOpen (row + 1, col) g,
                west = gridTraverse isOpen (row, col - 1) g
              }
        else Nothing

distanceTo :: SG.Position -> SparseGrid Char -> SparseGrid Int
distanceTo start@(startRow, startCol) g = distanceGrid
  where
    distanceGrid =
      SparseGrid
        { content =
            Map.insert start 0 $
              Map.fromSet distanceAt (Set.fromList $ SG.allPositions g),
          width = g.width,
          height = g.height,
          emptyValue = -1
        }

    distanceAt (row, col) | '#' <- SG.get (row, col) g = -1
    distanceAt (row, col) =
      let vertical
            | row < startRow = Just $ SG.get (row + 1, col) distanceGrid
            | row > startRow = Just $ SG.get (row - 1, col) distanceGrid
            | otherwise = Nothing
          horizontal
            | col < startCol = Just $ SG.get (row, col + 1) distanceGrid
            | col > startCol = Just $ SG.get (row, col - 1) distanceGrid
            | otherwise = Nothing
       in 1 + minimumDef (-2) (filter (>= 0) . catMaybes $ [vertical, horizontal])

parse :: ByteString -> (SG.Position, SparseGrid Char)
parse bs =
  let parsedGrid = SG.parseSparseGrid '.' bs
      (startRow, startCol) = headNote "Expected a single 'S' in input" $ Map.keys (Map.filter (== 'S') parsedGrid.content)
   in ( (startRow + 1, startCol + 1),
        parsedGrid
          & over #content (Map.filter (== '#'))
          & SG.addWall '#'
      )

example1 :: ByteString
example1 =
  BS.unlines
    [ "...........",
      ".....###.#.",
      ".###.##..#.",
      "..#.#...#..",
      "....#.#....",
      ".##..S####.",
      ".##..#...#.",
      ".......##..",
      ".##.#.####.",
      ".##..##.##.",
      "..........."
    ]
