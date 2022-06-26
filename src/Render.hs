module Render (drawApp) where

import Brick
  ( Widget,
    attrName,
    hBox,
    str,
    vBox,
    withAttr,
  )
import Brick.Widgets.Border
  ( border,
  )
import Cursor.Simple.List.NonEmpty
  ( nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
  )
import Data.Text
  ( unpack,
  )
import State
  ( AppState (AppState),
    Item (Item),
    ResourceName,
  )

drawApp :: AppState -> [Widget ResourceName]
drawApp (AppState items) =
  [ vBox $
      concat
        [ map (drawPath False) $ reverse $ nonEmptyCursorPrev items,
          [drawPath True $ nonEmptyCursorCurrent items],
          map (drawPath False) $ nonEmptyCursorNext items
        ]
  ]

drawPath :: Bool -> Item -> Widget ResourceName
drawPath isHighlighted (Item title _ currentValue _) =
  hBox
    [ border $ (attachAttrWhenHighlighted isHighlighted . str . unpack) title,
      border $ (attachAttrWhenHighlighted isHighlighted . str . unpack) currentValue
    ]

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr (attrName "selected")
  | otherwise = id