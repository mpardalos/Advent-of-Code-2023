{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_)
import Data.ByteString.Char8 qualified as BS
import Data.List (isInfixOf)
import Debug.Trace (traceMarkerIO)
import Solutions (Solution (..), displayAnswer, isSolvedAnswer, solutions)
import System.Clock
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

titleLength :: Int
titleLength =
  maximum
    [ length name
      | MkSolution name _ _ <- solutions
    ]

data AnchorType
  = Top
  | Middle
  | Bottom

printTableAnchor :: AnchorType -> IO ()
printTableAnchor anchorType =
  printf
    "%s─%s─%s────────%s───────────\n"
    startMarker
    (replicate titleLength '─')
    middleMarker
    middleMarker
  where
    startMarker = case anchorType of
      Top -> "╭"
      Middle -> "├"
      Bottom -> "╰"

    middleMarker = case anchorType of
      Top -> "┬"
      Middle -> "┼"
      Bottom -> "┴"

printLine :: String -> TimeSpec -> String -> IO ()
printLine name time answer =
  printf
    "│ %*s │ %6s │ %s \n"
    titleLength
    name
    formattedTime
    -- If the answer spans multiple lines, align it all in the right column of the table
    ( concatMap
        ( \case
            '\n' -> printf "\n│ %*s │        │ " titleLength ""
            c -> [c]
        )
        answer
    )
  where
    timeNanos :: Integer
    timeNanos = toNanoSecs time

    formattedTime :: String
    formattedTime
      | timeNanos == 0 = ""
      | timeNanos < 1e3 = printf "%d ns" timeNanos
      | timeNanos < 1e6 = printf "%d μs" (timeNanos `div` 1e3)
      | timeNanos < 1e9 = printf "%d ms" (timeNanos `div` 1e6)
      | otherwise = printf "%d  s" (timeNanos `div` 1e9)

main :: IO ()
main = do
  filterFun <-
    getArgs >>= \case
      [] -> return (const True)
      [filterString] -> return $ \(MkSolution name _ _) -> filterString `isInfixOf` name
      _ -> putStrLn "Too many arguments" >> exitFailure

  let filteredSolutions = filter filterFun solutions

  printTableAnchor Top
  times <- forM filteredSolutions $ \(MkSolution name solution inputFile) -> do
    try (BS.readFile ("data/" <> inputFile)) >>= \case
      Left (_ :: IOError) -> do
        printLine name 0 ""
        return 0
      Right input -> do
        startTime <- getTime Monotonic
        traceMarkerIO ("Begin " ++ name)
        answer <-
          try (evaluate (solution input)) >>= \case
            Left (e :: SomeException) -> pure (Just (show e))
            Right v
              | isSolvedAnswer v -> pure (Just (displayAnswer v))
              | otherwise -> pure Nothing
        traceMarkerIO ("End " ++ name)
        timeElapsed <- diffTimeSpec startTime <$> getTime Monotonic
        case answer of
          Nothing -> do
            printLine name 0 ""
            return 0
          Just s -> do
            printLine name timeElapsed s
            return timeElapsed
  printTableAnchor Middle
  printLine "Total time" (sum times) ""
  printTableAnchor Bottom
