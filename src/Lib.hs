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
import Graphics.Vty.Attributes (red)
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar, KDown, KUp))
import System.Directory
  ( getCurrentDirectory,
    getDirectoryContents,
  )
import System.Exit (die)

data TCMState = TCMState
  { tcmStatePaths :: NonEmptyCursor FilePath
  }
  deriving stock (Show, Eq)

buildInitialState :: IO TCMState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  case NE.nonEmpty contents of
    Nothing -> die "There are no directory contents to show."
    Just ne -> pure TCMState {tcmStatePaths = makeNonEmptyCursor ne}

tcmApp :: App TCMState e ResourceName
tcmApp =
  App
    { appDraw = drawTCM,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(attrName "selected", fg red)]
    }

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

drawTCM :: TCMState -> [Widget ResourceName]
drawTCM ts =
  let nec = tcmStatePaths ts
   in [ vBox $
          concat
            [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec,
              [drawPath True $ nonEmptyCursorCurrent nec],
              map (drawPath False) $ nonEmptyCursorNext nec
            ]
      ]

drawPath :: Bool -> FilePath -> Widget ResourceName
drawPath highlight =
  ( if highlight
      then withAttr $ attrName "selected"
      else id
  )
    . str

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e
  | VtyEvent vtye <- e =
    let nonEmptyCursor = tcmStatePaths s
     in case vtye of
          EvKey (KChar 'q') [] -> halt s
          EvKey (KChar 'd') [] -> continue $ deleteFirstEntry s
          EvKey KDown [] -> do
            case nonEmptyCursorSelectNext nonEmptyCursor of
              Nothing -> continue s
              Just nonEmptyCursor' -> continue $ s {tcmStatePaths = nonEmptyCursor'}
          EvKey KUp [] -> do
            case nonEmptyCursorSelectPrev nonEmptyCursor of
              Nothing -> continue s
              Just nonEmptyCursor' -> continue $ s {tcmStatePaths = nonEmptyCursor'}
          _ -> continue s
  | otherwise = continue s

deleteFirstEntry :: TCMState -> TCMState
deleteFirstEntry state =
  TCMState
    { tcmStatePaths = (tailOfNonEmptyCursor . tcmStatePaths) state
    }
  where
    tailOfNonEmptyCursor :: NonEmptyCursor a -> NonEmptyCursor a
    tailOfNonEmptyCursor = fromJust . nonEmptyCursorSelectIndex selectionPosition . makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor -- FIXME handle Maybe
      where
        selectionPosition = max 0 $ nonEmptyCursorSelection (tcmStatePaths state) - 1 -- FIXME handle Maybe