module Unit.Domain.ItemsCursorSpec where

import Domain.ItemsCursor
  ( ItemsCursor (MkItemsCursor),
    makeItemsCursor,
  )
import Infrastructure.Config
  ( ConfigItem (MkConfigItem),
    Pattern (MkPattern),
    TargetValue (MkTargetValue),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
  describe "Creating a list of items" $ do
    it "given an empty list should result in Nothing" $ do
      makeItemsCursor [] `shouldBe` Nothing
    it "given an a single item list should succeed" $ do
      let singleItemList = [MkConfigItem "" "" (MkPattern "") (MkTargetValue "") []]
       in makeItemsCursor singleItemList `shouldBe` Just (MkItemsCursor singleItemList)