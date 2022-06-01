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
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar))
import System.Directory
  ( getCurrentDirectory,
    getDirectoryContents,
  )

data TCMState = TCMState
  { tcmStatePaths :: [FilePath]
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
drawTCM ts = [vBox $ map drawPath $ tcmStatePaths ts]

drawPath :: FilePath -> Widget ResourceName
drawPath = str

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e
  | VtyEvent vtye <- e =
    case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 'd') [] -> continue $ deleteFirstEntry s
      _ -> continue s
  | otherwise = continue s

deleteFirstEntry :: TCMState -> TCMState
deleteFirstEntry (TCMState paths) = TCMState {tcmStatePaths = tail paths}