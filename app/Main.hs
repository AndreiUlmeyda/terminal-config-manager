module Main where

import Brick
import Graphics.Vty.Input.Events

data TCMState = TCMState deriving (Show, Eq)

main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  print endState

buildInitialState :: IO TCMState
buildInitialState = pure TCMState

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
drawTCM _ts = [ui]

handleEvent :: TCMState -> BrickEvent n e -> EventM n (Next TCMState)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s