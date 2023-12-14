{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Day14 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set

type Position = (Int, Int)

type Size = (Int, Int)

data Grid = Grid
  { roundRocks :: Set Position,
    squareRocks :: Set Position,
    size :: Size
  }
  deriving (Eq, Ord, Show)

data SquareOrRound = Square | Round
  deriving (Eq, Ord, Show)

part1 :: ByteString -> Int
part1 = sum . columnLoads . parse

columnLoads :: Grid -> [Int]
columnLoads Grid {roundRocks, squareRocks, size = (height, width)} =
  let cols :: [[(SquareOrRound, Int)]] =
        [ catMaybes
            [ if
                  | (row, col) `Set.member` squareRocks -> Just (Square, row)
                  | (row, col) `Set.member` roundRocks -> Just (Round, row)
                  | otherwise -> Nothing
              | row <- [0 .. height - 1]
            ]
          | col <- [0 .. width - 1]
        ]
   in map (columnLoad height) cols

columnLoad :: Int -> [(SquareOrRound, Int)] -> Int
columnLoad maxLoad column = go 0 0 column
  where
    go load row ((Round, _) : rest) = go (load + (maxLoad - row)) (row + 1) rest
    go load _ ((Square, row) : rest) = go load (row + 1) rest
    go load _ [] = load

part2 :: ByteString -> ()
part2 _ = ()

example1 :: Grid
example1 =
  parse . BS.unlines $
    [ "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    ]

parse :: ByteString -> Grid
parse input =
  let width = BS.length (BS.takeWhile (/= '\n') input)
      height = length (BS.lines input)
   in go (Grid Set.empty Set.empty (height, width)) (0, 0) (BS.unpack input)
  where
    go grid _ [] = grid
    go grid (row, col) (c : cs) = case c of
      'O' -> go grid {roundRocks = Set.insert (row, col) grid.roundRocks} (row, col + 1) cs
      '#' -> go grid {squareRocks = Set.insert (row, col) grid.squareRocks} (row, col + 1) cs
      '.' -> go grid (row, col + 1) cs
      '\n' -> go grid (row + 1, 0) cs
      _ -> error ("Invalid character: " ++ show c)
