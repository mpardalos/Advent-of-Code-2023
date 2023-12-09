{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Solutions (DisplaySolution (..), Solution (..), solutions) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Day1 (part1, part2)
import Day2 (part1, part2)
import Day3 (part1, part2)
import Day4 (part1, part2)
import Day5 (part1, part2)
import Day6 (part1, part2)
import Day7 (part1, part2)
import Day8 (part1, part2)
import Day9 (part1, part2)

data Solution where
  MkSolution :: (NFData a, DisplaySolution a) => String -> (ByteString -> a) -> FilePath -> Solution

class DisplaySolution a where
  displaySolution :: a -> String
  default displaySolution :: Show a => a -> String
  displaySolution = show

instance DisplaySolution Int

instance DisplaySolution Int64

instance DisplaySolution Double

instance DisplaySolution String where
  displaySolution = id

instance DisplaySolution () where
  displaySolution () = "-"

solutions :: [Solution]
solutions =
  [ MkSolution "Day 1 part 1" Day1.part1 "day1",
    MkSolution "Day 1 part 2" Day1.part2 "day1",
    MkSolution "Day 2 part 1" Day2.part1 "day2",
    MkSolution "Day 2 part 2" Day2.part2 "day2",
    MkSolution "Day 3 part 1" Day3.part1 "day3",
    MkSolution "Day 3 part 2" Day3.part2 "day3",
    MkSolution "Day 4 part 1" Day4.part1 "day4",
    MkSolution "Day 4 part 2" Day4.part2 "day4",
    MkSolution "Day 5 part 1" Day5.part1 "day5",
    MkSolution "Day 5 part 2" Day5.part2 "day5",
    MkSolution "Day 6 part 1" Day6.part1 "day6",
    MkSolution "Day 6 part 2" Day6.part2 "day6",
    MkSolution "Day 7 part 1" Day7.part1 "day7",
    MkSolution "Day 7 part 2" Day7.part2 "day7",
    MkSolution "Day 8 part 1" Day8.part1 "day8",
    MkSolution "Day 8 part 2" Day8.part2 "day8",
    MkSolution "Day 9 part 1" Day9.part1 "day9",
    MkSolution "Day 9 part 2" Day9.part2 "day9"
  ]
