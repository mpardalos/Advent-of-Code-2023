{-# LANGUAGE OverloadedStrings #-}

module Day24 (part1, part2) where

import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
  ( char,
    decimal,
    signed,
    skipSpace,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (mapMaybe)
import Util (uniquePairs, unsafeParse)

part1 :: ByteString -> Int
part1 input =
  let hailstones = parse input
      crossings = mapMaybe (uncurry cross) (uniquePairs hailstones)
   in length (filter (inBounds 2e14 4e14) crossings)

inBounds :: Double -> Double -> Vec3 -> Bool
inBounds from to Vec3 {x, y} =
  and
    [ x >= from,
      x <= to,
      y >= from,
      y <= to
    ]

cross :: HailStone -> HailStone -> Maybe Vec3
cross
  HailStone
    { position = Vec3 {x, y, z},
      velocity = Vec3 {x = dx, y = dy, z = dz}
    }
  HailStone
    { position = Vec3 {x = u, y = v},
      velocity = Vec3 {x = du, y = dv}
    } =
    do
      let t1 = (dv * (x - u) - du * (y - v)) / (dy * du - dx * dv)
          t2 = (dy * (u - x) - dx * (v - y)) / (dv * dx - du * dy)

      guard (dy * du /= dv * dx)
      guard (t1 >= 0)
      guard (t2 >= 0)

      return
        Vec3
          { x = x + t1 * dx,
            y = y + t1 * dy,
            z = z + t1 * dz
          }

part2 :: ByteString -> ()
part2 _ = ()

data HailStone = HailStone
  { position :: !Vec3,
    velocity :: !Vec3
  }

data Vec3 = Vec3 {x :: !Double, y :: !Double, z :: !Double}
  deriving (Show)

parse :: ByteString -> [HailStone]
parse = map (unsafeParse hailstoneLine) . BS.lines
  where
    hailstoneLine = do
      position <-
        Vec3
          <$> (fromIntegral @Int <$> decimal)
          <*> (fromIntegral @Int <$> ("," >> skipSpace >> decimal))
          <*> (fromIntegral @Int <$> ("," >> skipSpace >> decimal))

      _ <- skipSpace >> char '@' >> skipSpace

      velocity <-
        Vec3
          <$> (fromIntegral @Int <$> signed decimal)
          <*> (fromIntegral @Int <$> ("," >> skipSpace >> signed decimal))
          <*> (fromIntegral @Int <$> ("," >> skipSpace >> signed decimal))

      return (HailStone {position, velocity})

example1 =
  BS.unlines $
    [ "19, 13, 30 @ -2,  1, -2",
      "18, 19, 22 @ -1, -1, -2",
      "20, 25, 34 @ -2, -2, -4",
      "12, 31, 28 @ -1, -2, -1",
      "20, 19, 15 @  1, -5, -3"
    ]
