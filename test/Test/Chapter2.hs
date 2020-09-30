module Test.Chapter2
    ( chapter2
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter2


chapter2 :: Spec
chapter2 = describe "Chapter2" $ do
    describe "Chapter2Normal" $ it "" $ True `shouldBe` True
    describe "Chapter2Advanced" $ it "" $ True `shouldBe` True
