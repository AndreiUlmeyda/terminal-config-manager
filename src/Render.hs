module Render (drawApp, selectionStyling) where

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
import Config (ConfigItem (MkConfigItem))
import Cursor.Simple.List.NonEmpty
  ( nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
  )
import Graphics.Vty.Attributes
  ( Attr,
    cyan,
  )
import State
  ( AppState (MkAppState),
    ResourceName,
  )

drawApp :: AppState -> [Widget ResourceName]
drawApp (MkAppState items) =
  [ vBox $
      concat
        [ map (drawPath False) $ reverse $ nonEmptyCursorPrev items,
          [drawPath True $ nonEmptyCursorCurrent items],
          map (drawPath False) $ nonEmptyCursorNext items
        ]
  ]

drawPath :: Bool -> ConfigItem -> Widget ResourceName
drawPath isHighlighted (MkConfigItem title _ currentValue _) =
  hBox
    [ (attachAttrWhenHighlighted isHighlighted . txt) title,
      str " → ",
      (attachAttrWhenHighlighted isHighlighted . txt) currentValue
    ]

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr (attrName "selected")
  | otherwise = id

selectionStyling :: Attr
selectionStyling = fg cyan