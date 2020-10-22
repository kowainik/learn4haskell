{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Chapter1
    ( chapter1
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Hedgehog.Range as Range (linear)
import qualified Hedgehog.Gen as Gen (int)

import Test.Hspec.Hedgehog (hedgehog, (===), forAll)

import Chapter1


chapter1 :: Spec
chapter1 = describe "Chapter1" $ do
    chapter1normal
    chapter1advanced

reverseInt :: Int -> Int
reverseInt x = (*) (signum x) . read . reverse . show . abs  $ x

chapter1normal :: Spec
chapter1normal = describe "Chapter1Normal" $ do
    describe "Task4: next" $ do
        it "returns next Int for 42" $ next 42 `shouldBe` 43
        it "returns next Int for negative" $ next (-5) `shouldBe` (-4)
        it "returns next Int for 0" $ next 0 `shouldBe` 1
    describe "Task5: lastDigit" $ do
        it "last digit of 0" $ lastDigit 0 `shouldBe` 0
        it "last digit of 0 < x < 10" $ lastDigit 5 `shouldBe` 5
        it "last digit of 10 < x < 100" $ lastDigit 34 `shouldBe` 4
        it "last digit of 100 < x < 1000" $ lastDigit 341 `shouldBe` 1
        it "last digit of big num" $ lastDigit 1234789 `shouldBe` 9
        it "last digit of negative" $ lastDigit (-12) `shouldBe` 2
    describe "Task6: closestToZero" $ do
        it "both positive, 1st wins" $ closestToZero 100 200 `shouldBe` 100
        it "both positive, 2nd wins" $ closestToZero 200 100 `shouldBe` 100
        it "both negative, 2nd wins" $ closestToZero (-200) (-100) `shouldBe` (-100)
        it "both negative, 1st wins" $ closestToZero (-100) (-200) `shouldBe` (-100)
        it "with 0, 1st wins" $ closestToZero 0 (-200) `shouldBe` 0
        it "with 0, 2nd wins" $ closestToZero 10 0 `shouldBe` 0
        it "equals" $ closestToZero 42 42 `shouldBe` 42
        it "positive, negative, pos wins" $ closestToZero 11 (-12) `shouldBe` 11
        it "positive, negative, neg wins" $ closestToZero 12 (-11) `shouldBe` (-11)
    describe "Task7: mid" $ do
        it "positives up  " $ mid 10 20 30 `shouldBe` 20
        it "positives down" $ mid 30 20 10 `shouldBe` 20
        it "positives mix " $ mid 20 30 10 `shouldBe` 20
        it "negatives down" $ mid (-10) (-20) (-30) `shouldBe` (-20)
        it "negatives up  " $ mid (-30) (-20) (-10) `shouldBe` (-20)
        it "negatives mix " $ mid (-20) (-30) (-10) `shouldBe` (-20)
        it "all equal" $ mid 1 1 1 `shouldBe` 1
        it "all equal, except 1" $ mid 1 1 2 `shouldBe` 1
    describe "Task8: isVowel" $ do
        it "true for vowels" $ all isVowel "aeiou" `shouldBe` True
        it "false for non-vowels" $ isVowel 'c' `shouldBe` False
        it "false for symbol" $ isVowel '+' `shouldBe` False
    describe "Task9: sumLast2" $ do
        it "sumLast2 0" $ sumLast2 0 `shouldBe` 0
        it "sumLast2 0 < 10" $ sumLast2 9 `shouldBe` 9
        it "sumLast2 10 < 100" $ sumLast2 56 `shouldBe` 11
        it "sumLast2 100 < 1000" $ sumLast2 987 `shouldBe` 15
        it "sumLast2 0 > -10" $ sumLast2 (-9) `shouldBe` 9
        it "sumLast2 -10 > -100" $ sumLast2 (-56) `shouldBe` 11
        it "sumLast2 -100 > -1000" $ sumLast2 (-987) `shouldBe` 15
    describe "Task 4 & 5 : first and last digit" $ do
        it "last digit is the first digit of the reversed number" $ hedgehog $ do
            x <- forAll $ Gen.int (Range.linear (-200) 200)
            (firstDigit x :: Int) === (lastDigit (reverseInt x) :: Int)

chapter1advanced :: Spec
chapter1advanced = describe "Chapter1Advanced" $
    describe "Task 10*" $ do
        it "first digit 0" $ firstDigit 0 `shouldBe` 0
        it "first digit 0 < 10" $ firstDigit 9 `shouldBe` 9
        it "first digit 10 < 100" $ firstDigit 58 `shouldBe` 5
        it "first digit 100 < 1000" $ firstDigit 158 `shouldBe` 1
        it "first digit big" $ firstDigit 467321 `shouldBe` 4
        it "first digit 0 > -10" $ firstDigit (-9) `shouldBe` 9
        it "first digit -10 > -100" $ firstDigit (-58) `shouldBe` 5
        it "first digit -100 > -1000" $ firstDigit (-158) `shouldBe` 1
