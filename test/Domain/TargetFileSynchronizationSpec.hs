module Domain.TargetFileSynchronizationSpec where

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
  describe "" $ do
    it "" $ do
      extractValue (MkPattern "") "" (MkContent "") `shouldBe` MkTargetValue ""