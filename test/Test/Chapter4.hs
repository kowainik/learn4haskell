module Test.Chapter4
    ( chapter4
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter4 :: Spec
chapter4 = describe "Chapter4" $ do
    describe "Chapter4Normal" $ it "" $ True `shouldBe` True
    describe "Chapter4Advanced" $ it "" $ True `shouldBe` True
