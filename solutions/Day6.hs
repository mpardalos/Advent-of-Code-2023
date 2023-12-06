module Day6 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.Function ((&))
import Safe (fromJustNote)
import Util (readSpacedInts)

data Race = Race {time :: !Int, distanceRecord :: !Int}
  deriving (Show)

part1 :: ByteString -> Int
part1 input =
  parsePart1 input
    & map countSolutions
    & product

countSolutions :: Race -> Int
countSolutions Race {time, distanceRecord} =
  let fTime :: Double = fromIntegral time
      fDistanceRecord :: Double = fromIntegral distanceRecord
      maxSolution = (fTime + sqrt (fTime * fTime - 4 * fDistanceRecord)) / 2
      minSolution = (fTime - sqrt (fTime * fTime - 4 * fDistanceRecord)) / 2
   in floor maxSolution
        - ceiling minSolution
        + 1
        -- If either solution is exactly an integer then we need to remove it
        -- from the result, because we need to *beat* the record, not just match
        -- it
        - (if minSolution == fromIntegral @Int (floor minSolution) then 1 else 0)
        - (if maxSolution == fromIntegral @Int (floor maxSolution) then 1 else 0)

part2 :: ByteString -> Int
part2 input = countSolutions (parsePart2 input)

parsePart1 :: ByteString -> [Race]
parsePart1 input =
  let times = readSpacedInts . BS.dropWhile (not . isDigit) $ BS.lines input !! 0
      distances = readSpacedInts . BS.dropWhile (not . isDigit) $ BS.lines input !! 1
   in zipWith Race times distances

parsePart2 :: ByteString -> Race
parsePart2 input =
  let (time, _) =
        fromJustNote "Could not read time"
          . BS.readInt
          . BS.filter isDigit
          $ BS.lines input !! 0
      (distance, _) =
        fromJustNote "Could not read distance"
          . BS.readInt
          . BS.filter isDigit
          $ BS.lines input !! 1
   in Race time distance
