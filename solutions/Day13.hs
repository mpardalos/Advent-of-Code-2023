module Day13 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import Data.ByteString (ByteString)
import Data.SparseGrid (SparseGrid, sparseGridP)
import Util (unsafeParse, (<<$>>))

part1 :: ByteString -> ()
part1 _ = ()

part2 :: ByteString -> ()
part2 _ = ()

parse :: ByteString -> [SparseGrid ()]
parse = unsafeParse ((const () <<$>> sparseGridP '.') `sepBy` endOfLine)
