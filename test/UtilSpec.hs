module UtilSpec where

import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Util
  ( elementAfter,
    elementBefore,
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

-- it "given a first element equal to the target value should return the second element" $ do
--   elementAfter '3' "321" `shouldBe` '2'