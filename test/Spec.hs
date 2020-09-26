module Main (main) where

import Learn4haskell (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
