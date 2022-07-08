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
import Config (ConfigItem (MkConfigItem), Value (MkValue))
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

drawApp :: AppState -> [Widget ResourceName]
drawApp (MkAppState items) = [singleLayer]
  where
    singleLayer =
      (vBox . concat)
        [ map (drawPath False) (reverse (nonEmptyCursorPrev items)),
          [(withAttr (attrName "selected") . drawPath True . nonEmptyCursorCurrent) items],
          (map (drawPath False) . nonEmptyCursorNext) items
        ]

drawPath :: Bool -> ConfigItem -> Widget ResourceName
drawPath isHighlighted (MkConfigItem title _ _ (MkValue currentValue) _) =
  hBox
    [ txt title,
      str " → ",
      (attachAttrWhenHighlighted isHighlighted . txt) currentValue
    ]

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr (attrName "value")
  | otherwise = id

selectionStyling :: Attr
selectionStyling = withStyle currentAttr bold

valueStyling :: Attr
valueStyling = withStyle (fg cyan) bold