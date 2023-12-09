module Main where

import Control.Exception (try)
import Control.Monad (forM)
import Criterion.Main
  ( bench,
    bgroup,
    defaultConfig,
    defaultMainWith,
    nf,
    whnf,
  )
import Criterion.Types (Config (..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (catMaybes)
import Solutions (Solution (MkSolution), displayAnswer, isSolved, solutions)

main :: IO ()
main = do
  solutionsWithInputs :: [(Solution, ByteString)] <-
    fmap catMaybes $ forM solutions $ \solution@(MkSolution _ _ inputName) -> do
      inputResult <- try @IOError (BS.readFile ("data/" <> inputName))
      case (inputResult, isSolved solution) of
        (Right input, True) -> return (Just (solution, input))
        _ -> return Nothing

  defaultMainWith
    defaultConfig {reportFile = Just "benchmark.html"}
    [ bgroup
        "solutions"
        [ bench name (whnf solution input)
          | (MkSolution name solution _, input) <- solutionsWithInputs
        ],
      bench "All together" $
        nf
          (map (\(MkSolution _ solution _, input) -> displayAnswer $ solution input))
          solutionsWithInputs
    ]
