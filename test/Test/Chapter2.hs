{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Chapter2
    ( chapter2
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter2


chapter2 :: Spec
chapter2 = describe "Chapter2" $ do
    chapter2normal
    chapter2advanced

chapter2normal :: Spec
chapter2normal = describe "Chapter2Normal" $ do
    describe "Task3: subList" $ do
        it "range on empty list" $ subList 1 2 emptyInts `shouldBe` emptyInts
        it "range within" $ subList 2 5 [0..7] `shouldBe` [2..5]
        it "range 0 .. len" $ subList 0 10 [0..10] `shouldBe` [0..10]
        it "range negative" $ subList (-1) 5 [0..5] `shouldBe` []
        it "range overflow" $ subList 0 5 [0, 1] `shouldBe` [0, 1]
        it "range x > y" $ subList 5 2 [0..5] `shouldBe` []
        it "range equal" $ subList 0 0 [0..3] `shouldBe` [0]
    describe "Task4: firstHalf" $ do
        it "empty" $ firstHalf emptyInts `shouldBe` emptyInts
        it "even len" $ firstHalf [1,3,5,7] `shouldBe` [1, 3]
        it "odd len" $ firstHalf [1,3,5,7, 10] `shouldBe` [1, 3]
    describe "Task5: isThird42" $ do
        it "empty" $ isThird42 emptyInts `shouldBe` False
        it "one elem" $ isThird42 [42] `shouldBe` False
        it "two elem" $ isThird42 [1, 42] `shouldBe` False
        it "three elem with 42" $ isThird42 [1, 2, 42] `shouldBe` True
        it "three elem without 42" $ isThird42 [1, 2, 3] `shouldBe` False
        it "more than three elem with 42" $ isThird42 [1, 2, 42, 4, 5, 6] `shouldBe` True
        it "more than three elem without 42" $ isThird42 [1..10] `shouldBe` False
    describe "Task6: duplicate" $ do
        it "empty" $ duplicate emptyInts `shouldBe` emptyInts
        it "one elem" $ duplicate [0] `shouldBe` [0, 0]
        it "two elems" $ duplicate [-1, 0] `shouldBe` [-1, -1, 0, 0]
        it "many elems" $ duplicate [0..5] `shouldBe` [0,0,1,1,2,2,3,3,4,4,5,5]
    describe "Task7: takeEven" $ do
        it "empty" $ takeEven emptyInts `shouldBe` emptyInts
        it "one elem" $ takeEven [1] `shouldBe` [1]
        it "two elem" $ takeEven [1,2] `shouldBe` [1]
        it "many elems" $ takeEven [0 .. 10] `shouldBe` [0, 2 .. 10]
    describe "Task8: smartReplicate" $ do
        it "empty" $ smartReplicate emptyInts `shouldBe` emptyInts
        it "one elem: 0" $ smartReplicate [0] `shouldBe` emptyInts
        it "one elem: negative" $ smartReplicate [-5] `shouldBe` emptyInts
        it "many positive" $ smartReplicate [0..3] `shouldBe` [1, 2, 2, 3, 3, 3]
        it "many negative" $ smartReplicate [0, -1 .. -3] `shouldBe` []
    describe "Task9: contains" $ do
        it "empty" $ contains 0 ([] :: [[Int]]) `shouldBe` ([] :: [[Int]])
        it "one with elem" $ contains 0 [[5, 0, 1]] `shouldBe` [[5, 0, 1]]
        it "one with elem, one without" $ contains 0 [[5, 0, 1], [1..4]] `shouldBe` [[5, 0, 1]]
        it "one with elem, one without" $ contains 0 [[1..4], [5, 0, 1]] `shouldBe` [[5, 0, 1]]
        it "all without" $ contains 0 [[1..4], [5,4..1]] `shouldBe` ([] :: [[Int]])
        it "all with" $ contains 5 [[1..5], [6,5..1]] `shouldBe` [[1..5], [6,5..1]]
    describe "Task11: rotate" $ do
        it "empty list" $ rotate 5 emptyInts `shouldBe` emptyInts
        it "empty list with 0" $ rotate 0 emptyInts `shouldBe` emptyInts
        it "list rotate 0" $ rotate 0 [1..5] `shouldBe` [1..5]
        it "list rotate len" $ rotate 5 [1..5] `shouldBe` [1..5]
        it "list rotate n" $ rotate 3 [1..5] `shouldBe` [4,5,1,2,3]
        it "list rotate len + n" $ rotate 8 [1..5] `shouldBe` [4,5,1,2,3]
        it "empty on negative" $ rotate (-5) [1..5] `shouldBe` emptyInts

chapter2advanced :: Spec
chapter2advanced = describe "Chapter2Advanced" $
    describe "Task12*: rewind" $ do
        it "empty" $ rewind emptyInts `shouldBe` emptyInts
        it "one elem" $ rewind [1] `shouldBe` [1]
        it "many elems" $ rewind [1..10] `shouldBe` [10,9..1]
        it "many elems random" $ rewind [5,1,9,56,32,7,11] `shouldBe` [11,7,32,56,9,1,5]

emptyInts :: [Int]
emptyInts = []
