module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3

chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ do
        it "append Gold + +" $ append (Gold 10) (Gold 4) `shouldBe` (Gold 14)
        it "append Gold + -" $ append (Gold (-10)) (Gold 8) `shouldBe` (Gold (-2))
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
