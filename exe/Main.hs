{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
module Main where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List (tails)

day1part1 :: Text -> IO ()
day1part1 =
  print
    . sum
    . map (read @Int)
    . map (\l -> [fromJust (find isDigit l), fromJust (find isDigit (reverse l))])
    . lines
    . T.unpack

day1part2 :: Text -> IO ()
day1part2 =
  print
    . sum
    . map (\l -> head l * 10 + last l)
    . map (mapMaybe readDigit)
    . map tails
    . lines
    . T.unpack
readDigit :: String -> Maybe Int
readDigit ('z':'e':'r':'o':_) = Just 0
readDigit ('o':'n':'e':_) = Just 1
readDigit ('t':'w':'o':_) = Just 2
readDigit ('t':'h':'r':'e':'e':_) = Just 3
readDigit ('f':'o':'u':'r':_) = Just 4
readDigit ('f':'i':'v':'e':_) = Just 5
readDigit ('s':'i':'x':_) = Just 6
readDigit ('s':'e':'v':'e':'n':_) = Just 7
readDigit ('e':'i':'g':'h':'t':_) = Just 8
readDigit ('n':'i':'n':'e':_) = Just 9
readDigit ('0':_) = Just 0
readDigit ('1':_) = Just 1
readDigit ('2':_) = Just 2
readDigit ('3':_) = Just 3
readDigit ('4':_) = Just 4
readDigit ('5':_) = Just 5
readDigit ('6':_) = Just 6
readDigit ('7':_) = Just 7
readDigit ('8':_) = Just 8
readDigit ('9':_) = Just 9
readDigit _ = Nothing

main :: IO ()
main = do
  T.readFile "inputs/day1" >>= day1part1
  T.readFile "inputs/day1" >>= day1part2
