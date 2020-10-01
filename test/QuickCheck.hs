module Main (main) where

import Test.QuickCheck (quickCheckResult, isSuccess)
import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.Chapter1QC

main :: IO ()
main = do
  result <- quickCheckResult firstDigitRevLastDigit
  unless (isSuccess result) exitFailure
