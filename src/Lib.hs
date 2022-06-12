module Lib (buildInitialState, tcmApp) where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    continue,
    halt,
    showFirstCursor,
    str,
    vBox,
  )
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    makeNonEmptyCursor,
    nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
    nonEmptyCursorSelectNext,
    rebuildNonEmptyCursor,
  )
import Data.List.NonEmpty as NE (nonEmpty, tail)
import Data.Maybe (fromJust)
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar, KDown))
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
      appAttrMap = const $ attrMap mempty []
    }

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

ui :: Widget ResourceName
ui = str "Hello, world!"

drawTCM :: TCMState -> [Widget ResourceName]
drawTCM ts =
  let nec = tcmStatePaths ts
   in [ vBox $
          concat
            [ map drawPath $ reverse $ nonEmptyCursorPrev nec,
              [drawPath $ nonEmptyCursorCurrent nec],
              map drawPath $ nonEmptyCursorNext nec
            ]
      ]

drawPath :: FilePath -> Widget ResourceName
drawPath = str

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e
  | VtyEvent vtye <- e =
    case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 'd') [] -> continue $ deleteFirstEntry s
      EvKey KDown [] -> do
        let nonEmptyCursor = tcmStatePaths s
        case nonEmptyCursorSelectNext nonEmptyCursor of
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
    tailOfNonEmptyCursor = makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor