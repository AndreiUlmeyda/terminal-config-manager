module UserInterface.Render (drawTCM, selectionStyling, valueStyling) where

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
import Graphics.Vty.Attributes
  ( Attr,
    bold,
    currentAttr,
    cyan,
    withStyle,
  )
import Infrastructure.Config (ConfigItem (MkConfigItem), TargetValue (MkTargetValue))
import State
  ( AppState (MkAppState),
    ResourceName,
  )

-- | The rendering consists of a single layer, each line consists of an items title and current value. The selected line
--   is rendered boldface, the selected value, additionally, has a separate color.
drawTCM :: AppState -> [Widget ResourceName]
drawTCM (MkAppState items) = [singleLayer]
  where
    singleLayer = (vBox . concat) [itemsAboveSelected, selectedItem, itemsBelowSelected]
    itemsAboveSelected = map (drawItem NotHighlighted) (reverse (nonEmptyCursorPrev items))
    selectedItem = [(withAttr (attrName "selected") . drawItem Highlighted . nonEmptyCursorCurrent) items]
    itemsBelowSelected = (map (drawItem NotHighlighted) . nonEmptyCursorNext) items

data Highlighting = Highlighted | NotHighlighted deriving stock (Eq)

-- | Transform a config item into a simple widget and, optionally, add an attribute to it which is later used to
--   apply different styling to highlighted lines.
drawItem :: Highlighting -> ConfigItem -> Widget ResourceName
drawItem highlighting (MkConfigItem title _ _ (MkTargetValue currentValue) _) =
  hBox
    [ txt title,
      txt " → ",
      (attachAttrWhenHighlighted highlighting . txt) currentValue
    ]

-- | Conditionally attach an attribute which is later used to apply different styling to highlighted widgets.
attachAttrWhenHighlighted :: Highlighting -> Widget n -> Widget n
attachAttrWhenHighlighted Highlighted = withAttr (attrName "value")
attachAttrWhenHighlighted NotHighlighted = id

-- | Define the styling to be applied to selected items.
selectionStyling :: Attr
selectionStyling = withStyle currentAttr bold

-- | Define the styling to be applied to the value of a selected line.
valueStyling :: Attr
valueStyling = withStyle (fg cyan) bold