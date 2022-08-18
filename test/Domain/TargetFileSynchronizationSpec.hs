module Domain.TargetFileSynchronizationSpec where

import Data.Maybe
import Domain.TargetFileSynchronization
  ( extractValue,
  )
import Infrastructure.Config
  ( Pattern (..),
    TargetValue (MkTargetValue),
  )
import Infrastructure.FileModification
  ( Content (MkContent),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "Finding the current value inside of some file content" $ do
    it "given completely empty input, should result in Nothing" $ do
      extractValue (MkPattern "") "" (MkContent "") `shouldBe` Nothing
    it "given a pattern and nothing else, should result in Nothing" $ do
      extractValue (MkPattern "some {{value}} pattern") "" (MkContent "") `shouldBe` Nothing
    it "given file content and nothing else, should result in Nothing" $ do
      extractValue (MkPattern "") "" (MkContent "some target pattern") `shouldBe` Nothing
    it "given a value marker and nothing else, should result in Nothing" $ do
      extractValue (MkPattern "") "{{value}}" (MkContent "") `shouldBe` Nothing
    it "given a pattern without text in front or after the value marker, should result in the entire content" $ do
      let valueMarker = "{{value}}"
          content = "some content"
       in extractValue (MkPattern valueMarker) valueMarker (MkContent content) `shouldBe` Just (MkTargetValue content)