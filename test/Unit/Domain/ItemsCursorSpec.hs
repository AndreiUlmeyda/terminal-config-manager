module Unit.Domain.ItemsCursorSpec where

import Data.Maybe (fromJust)
import Domain.ItemsCursor
  ( ItemsCursor (..),
    changeElementUnderCursor,
    cursorDown,
    cursorUp,
    makeItemsCursor,
  )
import Infrastructure.Config
  ( ConfigItem (..),
    Pattern (MkPattern),
    TargetValue (MkTargetValue),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

item :: ConfigItem
item = MkConfigItem "" "" (MkPattern "") (MkTargetValue "") []

changedItem :: ConfigItem
changedItem = item {title = "new title"}

nonEmptyList :: [ConfigItem]
nonEmptyList = replicate 2 item

itemsCursor :: ItemsCursor
Just itemsCursor = makeItemsCursor nonEmptyList

spec :: Spec
spec = do
  describe "Creating a list of items" $ do
    it "given an empty list should result in Nothing" $
      makeItemsCursor [] `shouldBe` Nothing
    it "given a nonempty list should succeed and set the default cursor position to zero" $
      makeItemsCursor nonEmptyList `shouldBe` Just (MkItemsCursor nonEmptyList 0)
  describe "Moving the cursor" $ do
    it "upwards should increment the cursor position" $
      cursorDown itemsCursor `shouldBe` MkItemsCursor nonEmptyList 1
    it "upwards and then downwards should leave the itemCursor unchanged" $
      (cursorUp . cursorUp) itemsCursor `shouldBe` itemsCursor
    it "should stop at the upmost item" $
      (cursorPosition . cursorDown . cursorDown) itemsCursor `shouldBe` 1
    it "should stop at the bottommost item" $
      (cursorPosition . cursorUp) itemsCursor `shouldBe` 0
  describe "Changing the item at the cursor position" $ do
    it "given multiple items with the cursor position somewhere in the middle should modify the item at the correct position" $
      let titleChange item = item {title = "new title"}
       in changeElementUnderCursor titleChange (cursorDown itemsCursor) `shouldBe` (cursorDown . fromJust . makeItemsCursor) [item, changedItem]