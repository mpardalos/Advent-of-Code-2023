{-# LANGUAGE FieldSelectors #-}
module Day7 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Function (on, (&))
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Safe (fromJustNote)

data Part1

data Part2

data HandBid p = HandBid
  { hand :: !(Hand p),
    bid :: !Int
  }
  deriving (Show, Eq)

newtype Hand p = Hand [Card p]
  deriving (Eq)

newtype Card p = Card Char
  deriving (Show, Eq)

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving (Show, Eq, Ord)

part1 :: ByteString -> Int
part1 = sumEarnings @Part1 . parse

part2 :: ByteString -> Int
part2 = sumEarnings @Part2 . parse

sumEarnings :: QuestionPart p => [HandBid p] -> Int
sumEarnings handbids =
  sortBy (comparing hand) handbids
    & reverse
    & zip [1 ..]
    & map (\(rank, HandBid {bid}) -> rank * bid)
    & sum

parse :: ByteString -> [HandBid p]
parse input =
  BS.lines input
    & map
      ( \line -> case BS.words line of
          [handStr, bidStr] ->
            HandBid
              { hand = Hand . map Card . BS.unpack $ handStr,
                bid = fst . fromJustNote "Cannot read bid" $ BS.readInt bidStr
              }
          _ -> error ("Cannot read line: " ++ show line)
      )

classifyHand :: QuestionPart p => Hand p -> HandType
classifyHand hand =
  case groupSizes hand of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    (3 : _) -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    (2 : _) -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    ls -> error ("Invalid hand: " ++ show hand ++ "(" ++ show ls ++ ")")

instance QuestionPart p => Ord (Hand p) where
  compare (Hand l) (Hand r) =
    compare (classifyHand (Hand l)) (classifyHand (Hand r)) <> compare l r

instance QuestionPart p => Ord (Card p) where
  compare = compare `on` relativeRank

class QuestionPart p where
  groupSizes :: Hand p -> [Int]
  relativeRank :: Card p -> Int

instance QuestionPart Part1 where
  groupSizes (Hand cards) =
    sort cards -- Profiling indicates this sort is the most expensive part of the code (~10% of time and ~80% of allocs)
      & group
      & map length
      & sort
      & reverse

  relativeRank (Card c) = case c of
    'A' -> 1
    'K' -> 2
    'Q' -> 3
    'J' -> 4
    'T' -> 5
    '9' -> 6
    '8' -> 7
    '7' -> 8
    '6' -> 9
    '5' -> 10
    '4' -> 11
    '3' -> 12
    '2' -> 13
    _ -> error ("Invalid card: " ++ show c)

instance QuestionPart Part2 where
  groupSizes (Hand cards) =
    let jokers = length (filter (== Card 'J') cards)
        nonJokerCards = filter (/= Card 'J') cards
        rawGroupSizes = groupSizes @Part1 (coerce (Hand nonJokerCards))
     in case rawGroupSizes of
          (maxGroup : rest) -> maxGroup + jokers : rest
          [] -> [jokers]

  relativeRank (Card c) = case c of
    'A' -> 1
    'K' -> 2
    'Q' -> 3
    'T' -> 4
    '9' -> 5
    '8' -> 6
    '7' -> 7
    '6' -> 8
    '5' -> 9
    '4' -> 10
    '3' -> 11
    '2' -> 12
    'J' -> 13
    _ -> error ("Invalid card: " ++ show c)

instance Show (Hand p) where
  show (Hand hand) = [c | (Card c) <- hand]
