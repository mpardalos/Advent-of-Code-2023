module Day7 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function (on, (&))
import Data.List (group, sort)
import Safe (fromJustNote)

part1 :: ByteString -> Int
part1 input =
  sum
    [ rank * bid
      | (rank, HandBid {bid}) <- zip [1 ..] . reverse . sort $ parse input
    ]

part2 :: ByteString -> Int
part2 input = 0

parse :: ByteString -> [HandBid]
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

classifyHand :: Hand -> HandType
classifyHand (Hand hand) =
  case reverse . sort . map length . group . sort $ hand of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    (3 : _) -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    (2 : _) -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    ls -> error ("Invalid hand: " ++ show hand ++ "(" ++ show ls ++ ")")

data HandBid = HandBid
  { hand :: !Hand,
    bid :: !Int
  }
  deriving (Show, Eq, Ord)

newtype Card = Card Char
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

newtype Hand = Hand [Card]
  deriving (Eq)

instance Ord Hand where
  compare (Hand l) (Hand r) =
    compare (classifyHand (Hand l)) (classifyHand (Hand r)) <> compare l r

instance Ord Card where
  compare = compare `on` relativeRank
    where
      relativeRank :: Card -> Int
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

instance Show Hand where
  show (Hand hand) = "Hand " ++ [c | (Card c) <- hand]
