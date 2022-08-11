module Input (handleEvent) where

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

pattern KeyQWithoutModifiers :: Event
pattern KeyQWithoutModifiers <- EvKey (KChar 'q') []

pattern ArrowDownWithoutModifiers :: Event
pattern ArrowDownWithoutModifiers <- EvKey KDown []

pattern ArrowUpWithoutModifiers :: Event
pattern ArrowUpWithoutModifiers <- EvKey KUp []

pattern ArrowLeftWithoutModifiers :: Event
pattern ArrowLeftWithoutModifiers <- EvKey KLeft []

pattern ArrowRightWithoutModifiers :: Event
pattern ArrowRightWithoutModifiers <- EvKey KRight []

-- | Handle an event emitted by brick by unpacking the underlying vty event and passing it the appropriate handler.
handleEvent :: AppState -> BrickEvent n e -> NextAppState
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

-- | Handle a keyboard event, up and down keys for selection, left and right for changing the associated value and q to quit.
handleVtyEvent :: Event -> AppState -> NextAppState
handleVtyEvent event (MkAppState items) = case event of
  KeyQWithoutModifiers -> halt (MkAppState items)
  ArrowDownWithoutModifiers -> selectNextItem items
  ArrowUpWithoutModifiers -> selectPreviousItem items
  ArrowRightWithoutModifiers -> selectNextValue items
  ArrowLeftWithoutModifiers -> selectPreviousValue items
  _ -> continue (MkAppState items)