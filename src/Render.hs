module Render (drawApp, selectionStyling) where

import Brick
  ( Widget,
    attrName,
    bg,
    fg,
    hBox,
    on,
    str,
    txt,
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
import Graphics.Vty.Attributes (Attr, blue, cyan, green, white, yellow)
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
    [ (attachAttrWhenHighlighted isHighlighted . txt) title,
      str " => ",
      (attachAttrWhenHighlighted isHighlighted . txt) currentValue
    ]

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr (attrName "selected")
  | otherwise = id

selectionStyling :: Attr
selectionStyling = fg cyan