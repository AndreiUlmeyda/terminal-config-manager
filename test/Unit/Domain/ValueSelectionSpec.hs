{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.Domain.ValueSelectionSpec where

import Data.Text
  ( Text,
    append,
    pack,
  )
import Domain.ItemsCursor
  ( changeNthElement,
  )
import Domain.ValueSelection
  ( elementAfter,
    elementBefore,
    modify,
    valueMarker,
  )
import Infrastructure.Config
  ( Pattern (..),
    TargetValue (..),
  )
import Infrastructure.FileModification
  ( Content (..),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck
  ( modifyMaxSuccess,
    prop,
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary),
  )

-- import Test.QuickCheck.Instances

instance Arbitrary TargetValue where
  arbitrary = fmap MkTargetValue arbitrary -- oneof [pure (MkTargetValue ""), pure (MkTargetValue "a")]

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

-- | TODO incorporate the value marker with a high frequency
instance Arbitrary Pattern where
  arbitrary = fmap MkPattern arbitrary

instance Arbitrary Content where
  arbitrary = fmap MkContent arbitrary

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "selecting the element after a given value" $ do
    it "given an empty list should default to the target value" $
      elementAfter 'a' "" `shouldBe` 'a'
    it "given a list containing only the target value should return it" $
      elementAfter 'a' "a" `shouldBe` 'a'
    it "given a first element equal to the target value should return the second element" $
      elementAfter '3' "321" `shouldBe` '2'
    it "given the target value somewhere in the middle should return the successor" $
      elementAfter True [False, True, False] `shouldBe` False
    it "given the target value as the last element should wrap around to the first one" $
      elementAfter '3' "123" `shouldBe` '1'
    it "with the target value missing in the list should return the first element of the list" $
      elementAfter '3' "124" `shouldBe` '1'

  describe "selecting the element before a given value" $ do
    it "given an empty list should default to the target value" $
      elementBefore 'a' "" `shouldBe` 'a'
    it "given a list containing only the target value should return it" $
      elementBefore 'a' "a" `shouldBe` 'a'
    it "given a list containing the target somewhere in the middle, should return the element before it" $
      elementBefore 'a' "bac" `shouldBe` 'b'
    it "given the target value as the first element should wrap around to the last one" $
      elementBefore '1' "123" `shouldBe` '3'

  describe "changing the element at a certain index inside of a list" $ do
    it "given index 0, an empty list and id as a function should do nothing" $
      let emptyList = [] :: [Int]
       in changeNthElement 0 id emptyList `shouldBe` emptyList
    it "given a negative index, should do nothing" $
      let someList = [1, 2, 3] :: [Int]
          someFunction = (*) 2
          negativeIndex = -5
       in changeNthElement negativeIndex someFunction someList `shouldBe` someList
    it "given a valid index and a function should apply it at the appropriate index" $
      let someList = [1, 2, 3] :: [Int]
          someFunction = (*) 2
          validIndex = 1
       in changeNthElement validIndex someFunction someList `shouldBe` [1, 4, 3]
    it "given an index exceeding the length of the list should do nothing" $
      let someList = [1, 2, 3] :: [Int]
          someFunction = (+) 17
          outOfBoundsIndex = 500
       in changeNthElement outOfBoundsIndex someFunction someList `shouldBe` someList

  describe "modifying a string according to a search pattern and a new value" $ do
    prop "given empty content should leave the content unchanged" $
      \tva tvb pat -> modify tva tvb pat (MkContent "") == MkContent ""
    prop "given an empty pattern should leave the content unchanged" $
      \tva tvb cont -> modify tva tvb (MkPattern "") cont == cont
    prop "given that the old and new values are identical should leave the content unchanged" $
      \tva pat cont -> modify tva tva pat cont == cont
    it "given a pattern consisting only of the value marker should substitute all occurrences" $
      modify (MkTargetValue "ab") (MkTargetValue "xx") (MkPattern valueMarker) (MkContent "aabacaba") `shouldBe` MkContent "axxacxxa"
    it "given a prefix to the value marker should only modify the occurrence having said prefix" $
      modify (MkTargetValue "ab") (MkTargetValue "xx") (MkPattern ("z" `append` valueMarker)) (MkContent "azabacaba") `shouldBe` MkContent "azxxacaba"
    it "given a suffix to the value marker should only modify the occurrence having said suffix" $
      modify (MkTargetValue "ab") (MkTargetValue "xx") (MkPattern (valueMarker `append` "z")) (MkContent "aabzacaba") `shouldBe` MkContent "axxzacaba"
    it "given all empty inputs should result in empty content" $
      modify (MkTargetValue "") (MkTargetValue "") (MkPattern "") (MkContent "") `shouldBe` MkContent ""
    it "given all parameters being empty besides the first should not crash" $
      modify (MkTargetValue "a") (MkTargetValue "") (MkPattern "") (MkContent "") `shouldBe` MkContent ""
