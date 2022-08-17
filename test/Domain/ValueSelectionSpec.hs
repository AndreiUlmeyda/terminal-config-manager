module Domain.ValueSelectionSpec where

import Domain.ValueSelection
  ( changeNthElement,
    elementAfter,
    elementBefore,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "selecting the element after a given value" $ do
    it "given an empty list should default to the target value" $ do
      elementAfter 'a' "" `shouldBe` 'a'
    it "given a list containing only the target value should return it" $ do
      elementAfter 'a' "a" `shouldBe` 'a'
    it "given a first element equal to the target value should return the second element" $ do
      elementAfter '3' "321" `shouldBe` '2'
    it "given the target value somewhere in the middle should return the successor" $ do
      elementAfter True [False, True, False] `shouldBe` False
    it "given the target value as the last element should wrap around to the first one" $ do
      elementAfter '3' "123" `shouldBe` '1'

  describe "selecting the element before a given value" $ do
    it "given an empty list should default to the target value" $ do
      elementBefore 'a' "" `shouldBe` 'a'
    it "given a list containing only the target value should return it" $ do
      elementBefore 'a' "a" `shouldBe` 'a'
    it "given the target value as the first element should wrap around to the last one" $ do
      elementBefore '1' "123" `shouldBe` '3'

  describe "changing the element at a certain index inside of a list" $ do
    it "given index 0, an empty list and id as a function should do nothing" $ do
      let emptyList = [] :: [Int]
       in changeNthElement 0 id emptyList `shouldBe` emptyList
    it "given a negative index, should do nothing" $ do
      let someList = [1, 2, 3] :: [Int]
          someFunction = (*) 2
          negativeIndex = -5
       in changeNthElement negativeIndex someFunction someList `shouldBe` someList
    it "given a valid index and a function should apply it at the appropriate index" $ do
      let someList = [1, 2, 3] :: [Int]
          someFunction = (*) 2
          validIndex = 1
       in changeNthElement validIndex someFunction someList `shouldBe` [1, 4, 3]
    it "given an index exceeding the length of the list should do nothing" $ do
      let someList = [1, 2, 3] :: [Int]
          someFunction = (+) 17
          outOfBoundsIndex = 500
       in changeNthElement outOfBoundsIndex someFunction someList `shouldBe` someList