module Render (drawApp, selectionStyling, valueStyling) where

import Brick
  ( Widget,
    attrName,
    fg,
    hBox,
    str,
    txt,
    vBox,
    withAttr,
  )
import Config (ConfigItem (MkConfigItem), TargetValue (MkTargetValue))
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
import State
  ( AppState (MkAppState),
    ResourceName,
  )

-- The rendering consists of a single layer, each line consists of an items title and current value. The selected line
-- is rendered boldface, the selected value, additionally, has a separate color.
drawApp :: AppState -> [Widget ResourceName]
drawApp (MkAppState items) = [singleLayer]
  where
    singleLayer =
      (vBox . concat)
        [ map (drawPath NotHighlighted) (reverse (nonEmptyCursorPrev items)),
          [(withAttr (attrName "selected") . drawPath Highlighted . nonEmptyCursorCurrent) items],
          (map (drawPath NotHighlighted) . nonEmptyCursorNext) items
        ]

data Highlighting = Highlighted | NotHighlighted deriving stock (Eq)

drawPath :: Highlighting -> ConfigItem -> Widget ResourceName
drawPath highlighting (MkConfigItem title _ _ (MkTargetValue currentValue) _) =
  hBox
    [ txt title,
      str " → ",
      (attachAttrWhenHighlighted highlighting . txt) currentValue
    ]

attachAttrWhenHighlighted :: Highlighting -> Widget n -> Widget n
attachAttrWhenHighlighted Highlighted = withAttr (attrName "value")
attachAttrWhenHighlighted NotHighlighted = id

selectionStyling :: Attr
selectionStyling = withStyle currentAttr bold

valueStyling :: Attr
valueStyling = withStyle (fg cyan) bold