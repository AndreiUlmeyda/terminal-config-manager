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
    fadedStyling,
    attributeNameSelected,
    attributeNameValue,
    attributeNameFaded,
  )
where

import Brick
  ( Padding (Max),
    Widget,
    attrName,
    fg,
    hBox,
    padTop,
    txt,
    vBox,
    withAttr,
  )
import Data.Text
  ( Text,
    unpack,
  )
import Domain.ItemsCursor
  ( itemUnderCursor,
    itemsAfterCursor,
    itemsBeforeCursor,
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
    dim,
    withStyle,
  )
import Infrastructure.Config
  ( ConfigItem (MkConfigItem),
    TargetValue (MkTargetValue),
  )

attributeNameSelected :: Text
attributeNameSelected = "selected"

attributeNameValue :: Text
attributeNameValue = "value"

attributeNameFaded :: Text
attributeNameFaded = "faded"

titleValueSeparator :: Text
titleValueSeparator = " → "

-- | The rendering consists of a single layer, each line consists of an items
--   title and current value. The selected line is rendered boldface, the
--   selected value, additionally, has a separate color.
drawTCM :: AppState -> [Widget ResourceName]
drawTCM (MkAppState items) = [singleLayer]
  where
    singleLayer = (vBox . concat) [itemsAboveSelected, selectedItem, itemsBelowSelected, helpText]
    itemsAboveSelected = map (drawItem NotHighlighted) (itemsBeforeCursor items)
    selectedItem = [(withAttr ((attrName . unpack) attributeNameSelected) . drawItem Highlighted . itemUnderCursor) items]
    itemsBelowSelected = (map (drawItem NotHighlighted) . itemsAfterCursor) items
    helpText :: [Widget n]
    helpText = [padTop Max $ withAttr ((attrName . unpack) attributeNameFaded) helpTextWidget]

helpTextWidget :: Widget n
helpTextWidget = txt "↑/↓: navigate ←/→: modify q: quit"

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

-- | Define the styling to be applied to the instructions at the bottom.
fadedStyling :: Attr
fadedStyling = withStyle currentAttr dim