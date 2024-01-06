{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day20 (part1, part2) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 as Attoparsec hiding (parse)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (intercalate, unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import Text.Printf (printf)
import Util (graphvizView)

default (String)

part1 :: ByteString -> Int
part1 input =
  let graph = parse input
      buttonSignalCounts =
        unfoldr
          ( \gr ->
              let (signals, gr') = runButton gr
               in Just (countSignals signals, gr')
          )
          graph
      cumulativeSignalCounts :: [(Int, Int)] =
        scanl
          (\(l1, h1) (l2, h2) -> (l1 + l2, h1 + h2))
          (0, 0)
          buttonSignalCounts
      (lastLow, lastHigh) = cumulativeSignalCounts !! 1000
   in lastLow * lastHigh

countSignals :: [Signal] -> (Int, Int)
countSignals =
  foldr
    ( \Signal {level} (lows, highs) ->
        if level
          then (lows, highs + 1)
          else (lows + 1, highs)
    )
    (0, 0)

-- part1 :: ByteString -> Int
-- part1 input =
--   let graph = parse input
--       (resetIterations, (lowCount, highCount)) = buttonUntilReset graph
--    in (lowCount * 1000 `div` resetIterations) * (highCount * 1000 `div` resetIterations)

part2 :: ByteString -> ()
part2 _ = ()
-- part2 :: ByteString -> Int
-- part2 input =
--   let graph = parse input
--       buttonRuns = unfoldr (Just . runButton) graph
--    in length (dropWhile (not . any (\Signal {target} -> target == "rx")) buttonRuns)

-- buttonUntilReset :: NodeGraph -> (Int, (Int, Int))
-- buttonUntilReset initGraph = go 0 (0, 0) initGraph
--   where
--     go iters (lowCount, highCount) gr =
--       let ((lowCountAdd, highCountAdd), gr') = runButton gr
--           (lowCount', highCount') = (lowCount + lowCountAdd, highCount + highCountAdd)
--           iters' = iters + 1
--        in if all nodeIsReset gr'
--             then (iters', (lowCount', highCount'))
--             else go iters' (lowCount', highCount') gr'

--     nodeIsReset Broadcaster {} = True
--     nodeIsReset (Conjunction st _) = not (or st)
--     nodeIsReset (FlipFlop st _) = not st

runButton :: NodeGraph -> ([Signal], NodeGraph)
runButton initGraph =
  go
    initGraph
    ( Seq.singleton
        Signal
          { src = "button",
            target = "broadcaster",
            level = False
          }
    )
  where
    go gr Seq.Empty = ([], gr)
    go gr queue@(signal :<| _) =
      let (signals, finalGraph) = uncurry go (step gr queue)
       in (signal : signals, finalGraph)

showSignal :: Signal -> String
showSignal Signal {src, target, level} =
  printf
    "%s -%s> %s"
    (BS.unpack src)
    (if level then "high" else "low")
    (BS.unpack target)

step :: NodeGraph -> SignalQueue -> (NodeGraph, SignalQueue)
step gr (signal :<| queue) =
  let nodeName = signal.target
   in case Map.lookup nodeName gr of
        Nothing -> (gr, queue)
        Just (Broadcaster outputs) ->
          ( gr,
            queue
              >< Seq.fromList
                [ Signal
                    { src = nodeName,
                      target = output,
                      level = signal.level
                    }
                  | output <- outputs
                ]
          )
        Just (FlipFlop state outputs)
          | True <- signal.level ->
              (gr, queue)
          | False <- signal.level ->
              let newGraph = Map.insert nodeName (FlipFlop (not state) outputs) gr
               in ( newGraph,
                    queue
                      >< Seq.fromList
                        [ Signal
                            { src = nodeName,
                              target = output,
                              level = not state
                            }
                          | output <- outputs
                        ]
                  )
        Just (Conjunction state outputs) ->
          let newState = Map.insert signal.src signal.level state
              newGraph = Map.insert nodeName (Conjunction newState outputs) gr
           in -- !_ = unsafePerformIO (displayGraph newGraph)
              ( newGraph,
                queue
                  >< Seq.fromList
                    [ Signal
                        { src = nodeName,
                          target = output,
                          level = not (and newState)
                        }
                      | output <- outputs
                    ]
              )
step gr Seq.Empty = (gr, Seq.Empty)

type SignalQueue = Seq Signal

data Signal = Signal
  { src :: NodeName,
    target :: NodeName,
    level :: Bool
  }
  deriving (Show)

type NodeGraph = Map NodeName NodeInfo

type NodeName = ByteString

data NodeInfo
  = FlipFlop Bool [NodeName]
  | Conjunction (Map NodeName Bool) [NodeName]
  | Broadcaster [NodeName]
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

outgoing :: NodeInfo -> [NodeName]
outgoing (FlipFlop _ out) = out
outgoing (Conjunction _ out) = out
outgoing (Broadcaster out) = out

parse :: ByteString -> NodeGraph
parse bs = graph
  where
    graph = case parseOnly (line `sepBy` endOfLine) bs of
      Right nodes -> Map.fromList nodes
      Left e -> error ("Parse failure: " ++ show e)

    line :: Parser (NodeName, NodeInfo)
    line = broadcaster <|> flipFlop <|> conjunction

    nodeName = Attoparsec.takeWhile isAlpha_ascii

    broadcaster = do
      _ <- string "broadcaster -> "
      connections <- nodeName `sepBy` ", "
      return ("broadcaster", Broadcaster connections)

    flipFlop = do
      _ <- char '%'
      name <- nodeName
      _ <- string " -> "
      connections <- nodeName `sepBy` ", "
      return (name, FlipFlop False connections)

    conjunction = do
      _ <- char '&'
      name <- nodeName
      _ <- string " -> "
      connections <- nodeName `sepBy` ", "
      -- Cheeky value recursion
      -- Warning: Will hang if there are cyclic dependencies
      let state = fmap (const False) (Map.filter (\n -> name `elem` outgoing n) graph)
      return (name, Conjunction state connections)

toDot :: NodeGraph -> String
toDot graph = "digraph G { \n" <> unlines (map graphLine (Map.toList graph)) <> "\n}\n"
  where
    graphLine (name, node) =
      ( printf
          "  %s [label=\"%s\",shape=%s,%s];\n"
          (BS.unpack name)
          (label name node)
          (graphShape node)
          (fillColor node)
      )
        ++ connectionsLine name node

    label name (Conjunction state _) =
      intercalate
        "\\n"
        ( BS.unpack name
            : [ BS.unpack input <> ": " <> show st
                | (input, st) <- Map.toList state
              ]
        )
    label name _ = BS.unpack name

    graphShape, fillColor :: NodeInfo -> String
    graphShape Broadcaster {} = "ellipse"
    graphShape FlipFlop {} = "box"
    graphShape Conjunction {} = "diamond"

    fillColor (FlipFlop True _) = "style=filled,fillcolor=lightblue"
    fillColor (Conjunction st _) | or st = "style=filled,fillcolor=lightblue"
    fillColor _ = "style=\"\""

    connectionsLine name node =
      "  " ++ BS.unpack name ++ " -> {" ++ unwords (map BS.unpack (outgoing node)) ++ "};"

displayGraph :: NodeGraph -> IO ()
displayGraph = void . graphvizView . toDot

exampleGraph1 :: NodeGraph
exampleGraph1 = parse example1

example1 :: ByteString
example1 =
  BS.unlines $
    [ "broadcaster -> a, b, c",
      "%a -> b",
      "%b -> c",
      "%c -> inv",
      "&inv -> a"
    ]

exampleGraph2 :: NodeGraph
exampleGraph2 = parse example2

example2 :: ByteString
example2 =
  BS.unlines $
    [ "broadcaster -> a",
      "%a -> inv, con",
      "&inv -> b",
      "%b -> con",
      "&con -> output"
    ]
