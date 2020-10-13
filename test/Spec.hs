module Main (main) where

import Test.Hspec (hspec)

import Test.Chapter1
import Test.Chapter2
import Test.Chapter3
import Test.Chapter4


main :: IO ()
main = hspec $ do
    chapter1
    chapter2
    chapter3
    chapter4
