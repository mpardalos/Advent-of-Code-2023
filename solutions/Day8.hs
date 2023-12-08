{-# LANGUAGE OverloadedStrings #-}

module Day8 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map

type Directions = [Char]

type Node = ByteString

type Network = Map Node (Node, Node)

part1 :: ByteString -> Int
part1 input =
  let (directions, network) = parse input

      go :: (Int, ByteString) -> Directions -> (Int, ByteString)
      go (count, "ZZZ") _ = (count, "ZZZ")
      go (count, current) ('L': rest) = go (count + 1, fst (network Map.! current)) rest
      go (count, current) ('R': rest) = go (count + 1, snd (network Map.! current)) rest
      go _ _ = error "Invalid state"
   in fst $ go (0, "AAA") (cycle directions)
  where

part2 :: ByteString -> ()
part2 _ = ()

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
