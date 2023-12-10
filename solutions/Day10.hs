{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day10 (part1, part2, fancyDraw) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Position = (Int, Int)

type Pipe = Char

type Grid a = Map Position a

part1 :: ByteString -> Int
part1 input = length (traverseFromTo grid startPos startDir) `div` 2
  where
    grid = parse input
    startPos = findStartPos grid
    startDir = head $ directionsFrom grid startPos

part2 :: ByteString -> ()
part2 _ = ()

findStartPos :: Grid Pipe -> Position
findStartPos = Map.foldlWithKey (\acc pos c -> if c == 'S' then pos else acc) (-1, -1)

traverseFromTo :: Grid Pipe -> Position -> Position -> [Position]
traverseFromTo grid _ pos | Map.notMember pos grid = []
traverseFromTo grid prevPos@(prevRow, prevCol) pos@(row, col) = case grid Map.! pos of
  'S' -> [pos]
  '.' -> []
  -- row - prevRow should be +1 or -1, and indicates the direction we are
  -- travelling, so we can keep going the same way
  '|' -> pos : (traverseFromTo grid pos $ (row + (row - prevRow), col))
  '-' -> pos : (traverseFromTo grid pos $ (row, col + (col - prevCol)))
  -- We also need to keep travelling the same direction here, but the logic
  -- is a bit more complex because of the corner
  'L' -> pos : (traverseFromTo grid pos $ if prevPos == up then right else up)
  'J' -> pos : (traverseFromTo grid pos $ if prevPos == up then left else up)
  '7' -> pos : (traverseFromTo grid pos $ if prevPos == down then left else down)
  'F' -> pos : (traverseFromTo grid pos $ if prevPos == down then right else down)
  c -> error ("Invalid character: " ++ show c)
  where
    up = (row - 1, col)
    right = (row, col + 1)
    down = (row + 1, col)
    left = (row, col - 1)

directionsFrom :: Grid Pipe -> Position -> [Position]
directionsFrom grid (startRow, startCol) =
  map snd . filter fst $
    [ (startUp, (startRow - 1, startCol)),
      (startRight, (startRow, startCol + 1)),
      (startDown, (startRow + 1, startCol)),
      (startLeft, (startRow, startCol - 1))
    ]
  where
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

parse :: ByteString -> Grid Pipe
parse = Map.unions . parMap rseq (uncurry parseLine) . zip [0 ..] . BS.lines
  where
    parseLine row = fst . BS.foldl' (go row) (Map.empty, 0)

    go :: Int -> (Grid Pipe, Int) -> Char -> (Grid Pipe, Int)
    go row (!grid, !col) c
      | c `elem` "|-LJ7F.S" = (Map.insert (row, col) c grid, col + 1)
      | otherwise = error ("Invalid char: " ++ show c)

fancyDraw :: ByteString -> String
fancyDraw input =
  map fancify
    . BS.unpack
    . snd
    $ BS.mapAccumL step (0, 0) input
  where
    grid = parse input
    start = findStartPos grid
    startDir = head $ directionsFrom grid start
    loopPositions = traverseFromTo grid start startDir

    step :: Position -> Char -> (Position, Char)
    step pos@(row, col) c
      | '\n' <- c = ((row + 1, 0), '\n')
      | pos `elem` loopPositions = ((row, col + 1), c)
      | '.' <- c = ((row, col + 1), c)
      | otherwise = ((row, col + 1), ' ')

    fancify '|' = '│'
    fancify '-' = '─'
    fancify 'L' = '╰'
    fancify 'J' = '╯'
    fancify '7' = '╮'
    fancify 'F' = '╭'
    fancify '.' = '◯'
    fancify 'S' = 'S'
    fancify c = c
