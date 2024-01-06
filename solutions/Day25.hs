{-# LANGUAGE OverloadedStrings #-}

module Day25 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 as P (char, isAlpha_ascii, sepBy, takeWhile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Graph.Inductive (Gr, components, delEdges, edges, mkMapGraph)
import Data.List (nub)
import Debug.Trace (traceShow)
import Util (choose, unsafeParse)
import Util

part1 :: ByteString -> Int
part1 input =
  let gr = makeGraph (parse input)
      cutGraphs = [traceShow n (delEdges cutEdges gr) | (n, cutEdges) <- zip [1 ..] $ edges gr `choose` 3]
      (c1, c2) = head [(c1, c2) | [c1, c2] <- map components cutGraphs]
   in traceShow (length (edges gr)) (length c1 * length c2)

part2 :: ByteString -> ()
part2 _ = ()

makeGraph :: [(Name, [Name])] -> Gr Name ()
makeGraph connections =
  let nodes = nub [node | (from, tos) <- connections, node <- from : tos]
      edges = [(from, to, ()) | (from, tos) <- connections, to <- tos]
   in fst $ mkMapGraph nodes edges

type Name = ByteString

parse :: ByteString -> [(Name, [Name])]
parse = map (unsafeParse inputLine) . BS.lines
  where
    name = P.takeWhile isAlpha_ascii

    inputLine =
      (,)
        <$> name
        <*> (": " >> name `sepBy` char ' ')

example1 =
  BS.unlines $
    [ "jqt: rhn xhk nvd",
      "rsh: frs pzl lsr",
      "xhk: hfx",
      "cmg: qnr nvd lhk bvb",
      "rhn: xhk bvb hfx",
      "bvb: xhk hfx",
      "pzl: lsr hfx nvd",
      "qnr: nvd",
      "ntq: jqt hfx bvb xhk",
      "nvd: lhk",
      "lsr: lhk",
      "rzs: qnr cmg lsr rsh",
      "frs: qnr lhk lsr"
    ]
