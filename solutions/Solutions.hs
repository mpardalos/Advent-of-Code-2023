{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Solutions (DisplaySolution (..), Solution (..), solutions) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Day1 (part1, part2)
import Day10 (part1, part2)
import Day11 (part1, part2)
import Day12 (part1, part2)
import Day13 (part1, part2)
import Day14 (part1, part2)
import Day15 (part1, part2)
import Day16 (part1, part2)
import Day17 (part1, part2)
import Day18 (part1, part2)
import Day19 (part1, part2)
import Day2 (part1, part2)
import Day20 (part1, part2)
import Day21 (part1, part2)
import Day22 (part1, part2)
import Day23 (part1, part2)
import Day24 (part1, part2)
import Day25 (part1, part2)
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
    MkSolution "Day 9 part 2" Day9.part2 "day9",
    MkSolution "Day 10 part 1" Day10.part1 "day10",
    MkSolution "Day 10 part 2" Day10.part2 "day10",
    MkSolution "Day 11 part 1" Day11.part1 "day11",
    MkSolution "Day 11 part 2" Day11.part2 "day11",
    MkSolution "Day 12 part 1" Day12.part1 "day12",
    MkSolution "Day 12 part 2" Day12.part2 "day12",
    MkSolution "Day 13 part 1" Day13.part1 "day13",
    MkSolution "Day 13 part 2" Day13.part2 "day13",
    MkSolution "Day 14 part 1" Day14.part1 "day14",
    MkSolution "Day 14 part 2" Day14.part2 "day14",
    MkSolution "Day 15 part 1" Day15.part1 "day15",
    MkSolution "Day 15 part 2" Day15.part2 "day15",
    MkSolution "Day 16 part 1" Day16.part1 "day16",
    MkSolution "Day 16 part 2" Day16.part2 "day16",
    MkSolution "Day 17 part 1" Day17.part1 "day17",
    MkSolution "Day 17 part 2" Day17.part2 "day17",
    MkSolution "Day 18 part 1" Day18.part1 "day18",
    MkSolution "Day 18 part 2" Day18.part2 "day18",
    MkSolution "Day 19 part 1" Day19.part1 "day19",
    MkSolution "Day 19 part 2" Day19.part2 "day19",
    MkSolution "Day 20 part 1" Day20.part1 "day20",
    MkSolution "Day 20 part 2" Day20.part2 "day20",
    MkSolution "Day 21 part 1" Day21.part1 "day21",
    MkSolution "Day 21 part 2" Day21.part2 "day21",
    MkSolution "Day 22 part 1" Day22.part1 "day22",
    MkSolution "Day 22 part 2" Day22.part2 "day22",
    MkSolution "Day 23 part 1" Day23.part1 "day23",
    MkSolution "Day 23 part 2" Day23.part2 "day23",
    MkSolution "Day 24 part 1" Day24.part1 "day24",
    MkSolution "Day 24 part 2" Day24.part2 "day24",
    MkSolution "Day 25 part 1" Day25.part1 "day25",
    MkSolution "Day 25 part 2" Day25.part2 "day25"
  ]
