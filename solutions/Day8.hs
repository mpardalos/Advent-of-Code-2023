{-# LANGUAGE OverloadedStrings #-}

module Day8 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V

type Direction = Char

type Directions = [Direction]

type Node = ByteString

type Network = Map Node (Node, Node)

part1 :: ByteString -> Int
part1 input =
  let (directions, network) = parse input

      go :: (Int, ByteString) -> Directions -> (Int, ByteString)
      go (count, "ZZZ") _ = (count, "ZZZ")
      go (count, current) ('L' : rest) = go (count + 1, fst (network Map.! current)) rest
      go (count, current) ('R' : rest) = go (count + 1, snd (network Map.! current)) rest
      go _ _ = error "Invalid state"
   in fst $ go (0, "AAA") (cycle directions)

part2 :: ByteString -> Int
part2 input =
  let (directions, network) = parse input

      starts = V.fromList (filter ((== 'A') . BS.last) (Map.keys network))

      step :: Direction -> Node -> Node
      step 'L' current =
        let next = fst (network Map.! current)
         in if next == current then error "LOOP!" else next
      step 'R' current =
        let next = snd (network Map.! current)
         in if next == current then error "LOOP!" else next
      step c _ = error ("Invalid direction: " ++ show c)

      isFinal :: Node -> Bool
      isFinal = (== 'Z') . BS.last

      go :: (Int, Vector Node) -> Directions -> (Int, Vector Node)
      go (count, current) _ | all isFinal current = (count, current)
      go (count, current) (dir : rest) = go (count + 1, V.map (step dir) current) rest
      go _ _ = error "Invalid state"
   in fst $ go (0, starts) (cycle directions)

parse :: ByteString -> (Directions, Network)
parse input =
  let inputLines = BS.lines input
      directions = BS.unpack (head inputLines)

      mapLines = drop 2 inputLines
      network =
        Map.fromList $
          map
            ( \l ->
                let current = BS.take 3 l
                    left = BS.take 3 (BS.drop 7 l)
                    right = BS.take 3 (BS.drop 12 l)
                 in (current, (left, right))
            )
            mapLines
   in (directions, network)
