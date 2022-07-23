module Input (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    continue,
    halt,
  )
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )
import State
  ( AppState (MkAppState),
  )
import StateTransition
  ( ValueCyclingPolicy (..),
    ValueSelectionPolicy (..),
    cycleValues,
    next,
    previous,
    select,
    selectValueAndModifyTargetFile,
  )
import Util
  ( elementAfter,
    elementBefore,
  )

-- | Handle an event emitted by brick by unpacking the underlying vty event and passing it the appropriate handler.
handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

-- | Handle a keyboard event, up and down keys for selection, left and right for changing the associated value and q to quit.
handleVtyEvent :: Event -> AppState -> EventM n (Next AppState)
handleVtyEvent event (MkAppState items) = case event of
  EvKey (KChar 'q') [] -> halt (MkAppState items)
  EvKey KDown [] -> select next items
  EvKey KUp [] -> select previous items
  EvKey KRight [] -> selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementAfter))) items
  EvKey KLeft [] -> selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementBefore))) items
  _ -> continue (MkAppState items)