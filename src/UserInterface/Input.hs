-- |
-- Module      : Input
-- Description : Handle the keyboard user input.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module UserInterface.Input (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    continue,
    halt,
  )
import Domain.ItemSelection
  ( selectNextItem,
    selectPreviousItem,
  )
import Domain.State
  ( AppState (MkAppState),
    NextAppState,
  )
import Domain.ValueSelection
  ( selectNextValue,
    selectPreviousValue,
  )
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )

-- | Pattern synonym for the event raised when hitting q
pattern KeyQ :: Event
pattern KeyQ <- EvKey (KChar 'q') []

-- | Pattern synonym for the event raised when hitting arrow down
pattern ArrowDown :: Event
pattern ArrowDown <- EvKey KDown []

-- | Pattern synonym for the event raised when hitting arrow up
pattern ArrowUp :: Event
pattern ArrowUp <- EvKey KUp []

-- | Pattern synonym for the event raised when hitting arrow left
pattern ArrowLeft :: Event
pattern ArrowLeft <- EvKey KLeft []

-- | Pattern synonym for the event raised when hitting arrow right
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