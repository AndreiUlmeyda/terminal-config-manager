-- |
-- Module      : Render
-- Description : Expose a function to draw an application state to the screen
--   and two different styling, one for the description text and one for the value
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module UserInterface.Render
  ( drawTCM,
    selectionStyling,
    valueStyling,
    attributeNameSelected,
    attributeNameValue,
  )
where

import Brick
  ( Widget,
    attrName,
    fg,
    hBox,
    txt,
    vBox,
    withAttr,
  )
import Cursor.Simple.List.NonEmpty
  ( nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
  )
import Data.Text
  ( Text,
    unpack,
  )
import Domain.State
  ( AppState (MkAppState),
    ResourceName,
  )
import Graphics.Vty.Attributes
  ( Attr,
    bold,
    currentAttr,
    cyan,
    withStyle,
  )
import Infrastructure.Config (ConfigItem (MkConfigItem), TargetValue (MkTargetValue))

attributeNameSelected :: Text
attributeNameSelected = "selected"

attributeNameValue :: Text
attributeNameValue = "value"

titleValueSeparator :: Text
titleValueSeparator = " → "

-- | The rendering consists of a single layer, each line consists of an items
--   title and current value. The selected line is rendered boldface, the
--   selected value, additionally, has a separate color.
drawTCM :: AppState -> [Widget ResourceName]
drawTCM (MkAppState items) = [singleLayer]
  where
    singleLayer = (vBox . concat) [itemsAboveSelected, selectedItem, itemsBelowSelected]
    itemsAboveSelected = map (drawItem NotHighlighted) (reverse (nonEmptyCursorPrev items))
    selectedItem = [(withAttr ((attrName . unpack) attributeNameSelected) . drawItem Highlighted . nonEmptyCursorCurrent) items]
    itemsBelowSelected = (map (drawItem NotHighlighted) . nonEmptyCursorNext) items

-- | A type representing whether an item should be rendered highlighted or not.
data Highlighting = Highlighted | NotHighlighted deriving stock (Eq)

-- | Transform a config item into a simple widget and, optionally, add an
--   attribute to it which is later used to apply different styling to
--   highlighted lines.
drawItem :: Highlighting -> ConfigItem -> Widget ResourceName
drawItem highlighting (MkConfigItem title _ _ (MkTargetValue currentValue) _) =
  hBox
    [ txt title,
      txt titleValueSeparator,
      (attachAttrWhenHighlighted highlighting . txt) currentValue
    ]

-- | Conditionally attach an attribute which is later used to apply different
--   styling to highlighted widgets.
attachAttrWhenHighlighted :: Highlighting -> Widget n -> Widget n
attachAttrWhenHighlighted Highlighted = withAttr ((attrName . unpack) attributeNameValue)
attachAttrWhenHighlighted NotHighlighted = id

-- | Define the styling to be applied to selected items.
selectionStyling :: Attr
selectionStyling = withStyle currentAttr bold

-- | Define the styling to be applied to the value of a selected line.
valueStyling :: Attr
valueStyling = withStyle (fg cyan) bold