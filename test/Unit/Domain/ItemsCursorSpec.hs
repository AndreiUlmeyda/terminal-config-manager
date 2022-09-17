module Unit.Domain.ItemsCursorSpec where

import Domain.ItemsCursor
  ( ItemsCursor (MkItemsCursor),
    cursorUp,
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
    it "given a nonempty list should succeed and set the default cursor position to zero" $ do
      let nonEmptyList = [MkConfigItem "" "" (MkPattern "") (MkTargetValue "") []]
       in makeItemsCursor nonEmptyList `shouldBe` Just (MkItemsCursor nonEmptyList 0)
  describe "Moving the cursor" $ do
    it "upwards should increment the cursor position" $ do
      let item = MkConfigItem "" "" (MkPattern "") (MkTargetValue "") []
          nonEmptyList = replicate 2 item
          (Just itemsCursor) = makeItemsCursor nonEmptyList
       in cursorUp itemsCursor `shouldBe` MkItemsCursor nonEmptyList 1