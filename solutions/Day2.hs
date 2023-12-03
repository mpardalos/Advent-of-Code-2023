{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Data.Function ((&))

type Game = [Pull]

type Pull = [(Int, ByteString)]

maxRed, maxGreen, maxBlue :: Int
maxRed = 12
maxGreen = 13
maxBlue = 14

part1 :: ByteString -> Int
part1 input =
  map parseLine (BS.lines input)
    & zip [1 :: Int ..]
    & filter (gameValid . snd)
    & map fst
    & sum

part2 :: ByteString -> Int
part2 input =
  map parseLine (BS.lines input)
    & map gameMinimum
    & map (\(r, g, b) -> r * g * b)
    & sum

-- | The minimum number of red, green, and blue cubes (in that order) needed for
-- the game to be possible
gameMinimum :: Game -> (Int, Int, Int)
gameMinimum = foldl' considerCubeCount (0, 0, 0) . concat
  where
    considerCubeCount (r, g, b) (n, colour) =
      case colour of
        "red" -> (max r n, g, b)
        "green" -> (r, max g n, b)
        "blue" -> (r, g, max b n)
        _ -> error ("Invalid colour: " ++ BS.unpack colour)

gameValid :: Game -> Bool
gameValid = all (all countValid)
  where
    countValid :: (Int, ByteString) -> Bool
    countValid (n, "red") = n <= maxRed
    countValid (n, "green") = n <= maxGreen
    countValid (n, "blue") = n <= maxBlue
    countValid (_, colour) = error ("Invalid colour: " ++ BS.unpack colour)

parseLine :: ByteString -> Game
parseLine line =
  line
    & BS.dropWhile (/= ':')
    & BS.tail
    & BS.splitWith (== ';')
    & map (BS.splitWith (== ','))
    & map
      ( map
          ( \numColour ->
              case BS.readInt (BS.dropSpace numColour) of
                Just (num, rest) -> (num, BS.dropSpace rest)
                Nothing -> error ("Could not parse: " ++ BS.unpack numColour)
          )
      )
