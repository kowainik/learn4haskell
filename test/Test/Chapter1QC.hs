{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Chapter1QC
    ( firstDigitRevLastDigit
    ) where

import Chapter1

firstDigitRevLastDigit :: Int -> Bool
firstDigitRevLastDigit x = firstDig == lastDig
    where
      lastDig = lastDigit $ (read  (reverse $ show $ abs x) :: Int)
      firstDig = firstDigit x

