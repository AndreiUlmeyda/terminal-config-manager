module Main where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    continue,
    defaultMain,
    halt,
    showFirstCursor,
    str,
    vBox,
  )
import Cursor.Simple.List.NonEmpty (NonEmptyCursor, makeNonEmptyCursor, nonEmptyCursorSelectNext)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty as NE (nonEmpty)
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar, KDown))
import System.Directory
  ( getCurrentDirectory,
    getDirectoryContents,
  )
import System.Exit (die)

data TCMState = TCMState
  { tcmStatePaths :: NonEmptyCursor FilePath
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  print endState

buildInitialState :: IO TCMState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  case NE.nonEmpty contents of
    Nothing -> die "There are no directory contents to show."
    Just ne -> makeNonEmptyCursor ne
  pure TCMState {tcmStatePaths = contents}

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
  deriving (Show, Eq, Ord)

ui :: Widget ResourceName
ui = str "Hello, world!"

drawTCM :: TCMState -> [Widget ResourceName]
drawTCM ts = [vBox $ Prelude.map drawPath $ tcmStatePaths ts]

drawPath :: NonEmptyCursor FilePath -> Widget ResourceName
drawPath (NonEmptyCursor filePath) = str filePath

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e
  | VtyEvent vtye <- e =
    case vtye of
      EvKey (KChar 'q') noModifiers -> halt s
      -- EvKey (KChar 'd') noModifiers -> continue $ deleteFirstEntry s
      EvKey KDown noModifiers -> do
        let nonEmptyCursor = tcmStatePaths s
        case nonEmptyCursorSelectNext nonEmptyCursor of
          Nothing -> continue s
          Just nonEmptyCursor' -> continue $ s {tcmStatePaths = nonEmptyCursor'}
      _ -> continue s
  | otherwise = continue s

-- deleteFirstEntry :: TCMState -> TCMState
-- deleteFirstEntry (TCMState paths) = TCMState {tcmStatePaths = tail paths}

noModifiers = []