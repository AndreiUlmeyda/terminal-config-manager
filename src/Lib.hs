module Lib (buildInitialState, tcmApp) where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    attrName,
    continue,
    fg,
    halt,
    showFirstCursor,
    str,
    vBox,
    withAttr,
  )
import Brick.Widgets.Border (border)
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    makeNonEmptyCursor,
    nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
    nonEmptyCursorSelectIndex,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelection,
    rebuildNonEmptyCursor,
  )
import Data.List.NonEmpty as NE (nonEmpty, tail)
import Data.Maybe (fromJust)
import Data.Text hiding (concat, lines, map, reverse, take)
import Graphics.Vty.Attributes (currentAttr, cyan)
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar, KDown, KUp))
import System.Directory
  ( getCurrentDirectory,
    getDirectoryContents,
  )
import System.Exit (die)
import System.IO (Handle, IOMode (ReadMode), hClose, openFile, readFile, withFile)

data Item = Item Text deriving stock (Show, Eq)

data AppState = AppState (NonEmptyCursor Item) deriving stock (Show, Eq)

buildInitialState :: IO AppState
buildInitialState = do
  contents <- fmap lines $ readFile "test/data/config"
  let items = map (Item . pack) contents
   in case NE.nonEmpty items of
        Nothing -> die "There are no directory contents to show."
        Just ne -> pure $ AppState (makeNonEmptyCursor ne)

tcmApp :: App AppState e ResourceName
tcmApp =
  App
    { appDraw = drawTCM,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap currentAttr [(attrName "selected", fg cyan)]
    }

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

drawTCM :: AppState -> [Widget ResourceName]
drawTCM (AppState items) =
  [ vBox $
      concat
        [ map (drawPath False) $ reverse $ nonEmptyCursorPrev items,
          [border $ drawPath True $ nonEmptyCursorCurrent items],
          map (drawPath False) $ nonEmptyCursorNext items
        ]
  ]

drawPath :: Bool -> Item -> Widget ResourceName
drawPath isHighlighted (Item content) = (attachAttrWhenHighlighted isHighlighted . str . unpack) content

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr $ attrName "selected"
  | otherwise = id

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent (AppState items) e
  | VtyEvent vtye <- e =
      case vtye of
        EvKey (KChar 'q') [] -> halt (AppState items)
        EvKey (KChar 'd') [] -> continue $ deleteFirstEntry (AppState items)
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext items of
            Nothing -> continue (AppState items)
            Just nonEmptyCursor' -> continue $ AppState nonEmptyCursor'
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev items of
            Nothing -> continue (AppState items)
            Just nonEmptyCursor' -> continue $ AppState nonEmptyCursor'
        _ -> continue (AppState items)
  | otherwise = continue (AppState items)

deleteFirstEntry :: AppState -> AppState
deleteFirstEntry (AppState items) = (AppState . tailOfNonEmptyCursor) items
  where
    tailOfNonEmptyCursor :: NonEmptyCursor a -> NonEmptyCursor a
    tailOfNonEmptyCursor = fromJust . nonEmptyCursorSelectIndex selectionPosition . makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor -- FIXME handle Maybe
    selectionPosition = max 0 $ nonEmptyCursorSelection items - 1 -- FIXME handle Maybe