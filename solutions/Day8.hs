{-# LANGUAGE OverloadedStrings #-}

module Day8 (part1, part2) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map

type Direction = Char

type Directions = [Direction]

type Node = ByteString

type Network = Map Node (Node, Node)

part1 :: ByteString -> Int
part1 input =
  let (directions, network) = parse input
   in fst $ distanceToFinal (== "ZZZ") network directions "AAA"

part2 :: ByteString -> Int
part2 input =
  let (directions, network) = parse input
   in Map.keys network
        & filter ((== 'A') . BS.last)
        & parMap rseq (distanceToFinal ((== 'Z') . BS.last) network directions)
        & map fst
        & foldl1 lcm

distanceToFinal :: (Node -> Bool) -> Network -> Directions -> Node -> (Int, Node)
distanceToFinal isFinal network directions start = go (0, start) (cycle directions)
  where
    go :: (Int, Node) -> Directions -> (Int, Node)
    go _ [] = error "Out of directions"
    go (count, current) (c : rest)
      | isFinal current = (count, current)
      | 'L' <- c = go (count + 1, fst (network Map.! current)) rest
      | 'R' <- c = go (count + 1, snd (network Map.! current)) rest
      | otherwise = error "Invalid state"

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
