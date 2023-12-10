{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day10 (part1, part2) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Position = (Int, Int)

type Direction = (Int, Int)

type Pipe = Char

type Grid a = Map Position a

part1 :: ByteString -> Int
part1 input =
  let goingOneWay = go 1 startPos start1
      goingTheOtherWay = go 1 startPos start2
   in -- in grid `seq`
      --      (goingOneWay `par` goingTheOtherWay) `seq`
      --        (maximum (Map.unionWith min goingOneWay goingTheOtherWay))
      maximum (Map.unionWith min goingOneWay goingTheOtherWay)
  where
    grid = parse input

    go :: Int -> Position -> Position -> Grid Int
    go _ _ pos | Map.notMember pos grid = Map.empty
    go d prevPos@(prevRow, prevCol) pos@(row, col) = case grid Map.! pos of
      'S' -> Map.empty
      '.' -> Map.empty
      -- row - prevRow should be +1 or -1, and indicates the direction we are
      -- travelling, so we can keep going the same way
      '|' -> Map.insert pos d $ go (d + 1) pos $ (row + (row - prevRow), col)
      '-' -> Map.insert pos d $ go (d + 1) pos $ (row, col + (col - prevCol))
      -- We also need to keep travelling the same direction here, but the logic
      -- is a bit more complex because of the corner
      'L' -> Map.insert pos d $ go (d + 1) pos $ if prevPos == up then right else up
      'J' -> Map.insert pos d $ go (d + 1) pos $ if prevPos == up then left else up
      '7' -> Map.insert pos d $ go (d + 1) pos $ if prevPos == down then left else down
      'F' -> Map.insert pos d $ go (d + 1) pos $ if prevPos == down then right else down
      c -> error ("Invalid character: " ++ show c)
      where
        up = (row - 1, col)
        right = (row, col + 1)
        down = (row + 1, col)
        left = (row, col - 1)

    startPos@(startRow, startCol) = Map.foldlWithKey (\acc pos c -> if c == 'S' then pos else acc) (-1, -1) grid

    startUp = case Map.lookup (startRow - 1, startCol) grid of
      Nothing -> False
      Just c -> c `elem` "|7F"

    startRight = case Map.lookup (startRow, startCol + 1) grid of
      Nothing -> False
      Just c -> c `elem` "-J7"

    startDown = case Map.lookup (startRow + 1, startCol) grid of
      Nothing -> False
      Just c -> c `elem` "|LJ"

    startLeft = case Map.lookup (startRow - 1, startCol) grid of
      Nothing -> False
      Just c -> c `elem` "-LF"

    start1, start2 :: Direction
    [(True, start1), (True, start2)] =
      filter fst $
        [ (startUp, (startRow - 1, startCol)),
          (startRight, (startRow, startCol + 1)),
          (startDown, (startRow + 1, startCol)),
          (startLeft, (startRow, startCol - 1))
        ]

part2 :: ByteString -> ()
part2 _ = ()

parse :: ByteString -> Grid Pipe
parse = Map.unions . parMap rseq (uncurry parseLine) . zip [0 ..] . BS.lines
  where
    parseLine row = fst . BS.foldl' (go row) (Map.empty, 0)

    go :: Int -> (Grid Pipe, Int) -> Char -> (Grid Pipe, Int)
    go row (!grid, !col) c
      | c `elem` "|-LJ7F.S" = (Map.insert (row, col) c grid, col + 1)
      | otherwise = error ("Invalid char: " ++ show c)
