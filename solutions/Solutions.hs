{-# LANGUAGE FlexibleInstances #-}

module Solutions (DisplaySolution (..), Solution (..), solutions) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Day1 (part1, part2)
import Day2 (part1, part2)
import Day3 (part1, part2)
import Day4 (part1, part2)

data Solution where
  MkSolution :: (NFData a, DisplaySolution a) => String -> (ByteString -> a) -> FilePath -> Solution

class DisplaySolution a where
  displaySolution :: a -> String

instance DisplaySolution Int where
  displaySolution = show

instance DisplaySolution Double where
  displaySolution = show

instance DisplaySolution String where
  displaySolution = id

solutions :: [Solution]
solutions =
  [ MkSolution "Day 1 part 1" Day1.part1 "day1",
    MkSolution "Day 1 part 2" Day1.part2 "day1",
    MkSolution "Day 2 part 1" Day2.part1 "day2",
    MkSolution "Day 2 part 2" Day2.part2 "day2",
    MkSolution "Day 3 part 1" Day3.part1 "day3",
    MkSolution "Day 3 part 2" Day3.part2 "day3",
    MkSolution "Day 4 part 1" Day4.part1 "day4",
    MkSolution "Day 4 part 2" Day4.part2 "day4"
  ]
