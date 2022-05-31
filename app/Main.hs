module Main where

import Brick
import Graphics.Vty.Input.Events
import System.Directory

data TCMState =
  TCMState { tcmStatePaths :: [FilePath]
  } deriving (Show, Eq)

main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  print endState

buildInitialState :: IO TCMState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  pure TCMState { tcmStatePaths = contents}

tcmApp :: App TCMState e ResourceName
tcmApp =
  App
    { appDraw = drawTCM
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

ui :: Widget ResourceName
ui = str "Hello, world!"

drawTCM :: TCMState -> [Widget ResourceName]
drawTCM ts = [vBox $ map drawPath $ tcmStatePaths ts]

drawPath :: FilePath -> Widget ResourceName
drawPath = str

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar 'd') [] -> continue $ deleteFirstEntry s
        _ -> continue s
    _ -> continue s

deleteFirstEntry :: TCMState -> TCMState
deleteFirstEntry s = TCMState { tcmStatePaths = tail (tcmStatePaths s) }