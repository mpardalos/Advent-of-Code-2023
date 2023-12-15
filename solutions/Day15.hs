{-# LANGUAGE LambdaCase #-}

module Day15 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (foldl')
import Safe (fromJustNote)

part1 :: ByteString -> Int
part1 input =
  part1Parse input
    & map hash
    & sum

part1Parse :: ByteString -> [ByteString]
part1Parse = BS.split ',' . BS.filter (/= '\n')

hash :: ByteString -> Int
hash = BS.foldl' (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

part2 :: ByteString -> Int
part2 input =
  part2Parse input
    & foldl' (flip applyInstruction) IntMap.empty
    & focusingPower

data Instruction
  = Insert !ByteString !Int
  | Remove !ByteString
  deriving (Eq, Ord, Show)

type BoxMap = IntMap [(ByteString, Int)]

part2Parse :: ByteString -> [Instruction]
part2Parse = map readInstruction . part1Parse
  where
    readInstruction bs =
      let label = BS.takeWhile (`notElem` ['-', '=']) bs
          rest = BS.dropWhile (`notElem` ['-', '=']) bs
       in case BS.head rest of
            '-' -> Remove label
            '=' ->
              Insert
                label
                ( BS.tail rest
                    & BS.readInt
                    & fromJustNote "Could not parse focal length"
                    & fst
                )
            c -> error ("Invalid operation character: " ++ show c)

applyInstruction :: Instruction -> BoxMap -> BoxMap
applyInstruction (Remove label) =
  IntMap.adjust
    (filter ((/= label) . fst))
    (hash label)
applyInstruction (Insert label focalLength) =
  adjustOrInsert
    insert
    [(label, focalLength)]
    (hash label)
  where
    adjustOrInsert :: (v -> v) -> v -> IntMap.Key -> IntMap v -> IntMap v
    adjustOrInsert f v =
      IntMap.alter
        ( \case
            Nothing -> Just v
            Just v' -> Just (f v')
        )

    insert xs
      | label `elem` map fst xs =
          [ if l == label
              then (label, focalLength)
              else (l, f)
            | (l, f) <- xs
          ]
      | otherwise = xs ++ [(label, focalLength)]

focusingPower :: BoxMap -> Int
focusingPower boxMap =
  sum . concat $
    [ [ (box + 1) * (lensSlot + 1) * focalLength
        | (lensSlot, (_, focalLength)) <- zip [0 ..] lenses
      ]
      | (box, lenses) <- IntMap.toList boxMap
    ]
