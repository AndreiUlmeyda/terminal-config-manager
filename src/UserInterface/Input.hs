module UserInterface.Input (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    continue,
    halt,
  )
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )
import State
  ( AppState (MkAppState),
    NextAppState,
  )
import StateTransition
  ( selectNextItem,
    selectNextValue,
    selectPreviousItem,
    selectPreviousValue,
  )

pattern KeyQ :: Event
pattern KeyQ <- EvKey (KChar 'q') []

pattern ArrowDown :: Event
pattern ArrowDown <- EvKey KDown []

pattern ArrowUp :: Event
pattern ArrowUp <- EvKey KUp []

pattern ArrowLeft :: Event
pattern ArrowLeft <- EvKey KLeft []

pattern ArrowRight :: Event
pattern ArrowRight <- EvKey KRight []

-- | Handle an event emitted by brick by unpacking the underlying vty event and passing it the appropriate handler.
handleEvent :: AppState -> BrickEvent n e -> NextAppState
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

-- | Handle a keyboard event, up and down keys for selection, left and right for changing the associated value and q to quit.
handleVtyEvent :: Event -> AppState -> NextAppState
handleVtyEvent event (MkAppState items) = case event of
  KeyQ -> halt (MkAppState items)
  ArrowDown -> selectNextItem items
  ArrowUp -> selectPreviousItem items
  ArrowRight -> selectNextValue items -- >>= writeChangesToFile
  ArrowLeft -> selectPreviousValue items -- >>= writeChangesToFile
  _ -> continue (MkAppState items)