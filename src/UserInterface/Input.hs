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
    halt,
  )
import Control.Monad.IO.Class
  ( MonadIO (liftIO),
  )
import Control.Monad.State
  ( get,
    modify,
    put,
  )
import Domain.ItemSelection
  ( selectNextItem,
    selectPreviousItem,
  )
import Domain.State
  ( AppState (..),
    NextAppState,
    ResourceName,
  )
import Domain.ValueSelection
  ( selectNextValue,
    selectPreviousValue,
  )
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )
import Infrastructure.FsReadIO
  ( FsReadIO (runFsReadIO),
  )

-- | Pattern synonym for the event raised when hitting q
pattern KeyQ :: Event
pattern KeyQ = EvKey (KChar 'q') []

-- | Pattern synonym for the event raised when hitting arrow down
pattern ArrowDown :: Event
pattern ArrowDown = EvKey KDown []

-- | Pattern synonym for the event raised when hitting arrow up
pattern ArrowUp :: Event
pattern ArrowUp = EvKey KUp []

-- | Pattern synonym for the event raised when hitting arrow left
pattern ArrowLeft :: Event
pattern ArrowLeft = EvKey KLeft []

-- | Pattern synonym for the event raised when hitting arrow right
pattern ArrowRight :: Event
pattern ArrowRight = EvKey KRight []

-- | Handle an event emitted by brick by unpacking the underlying vty event and
--   passing it to the appropriate handler.
handleEvent :: BrickEvent ResourceName e -> NextAppState
handleEvent event
  | VtyEvent vtye <- event = handleVtyEvent vtye
  | otherwise = continue

-- | Handle a keyboard event, up and down keys for selection, left and right for
--   changing the associated value and q to quit.
handleVtyEvent :: Event -> NextAppState
handleVtyEvent KeyQ = halt
handleVtyEvent ArrowDown = modify selectPreviousItem
handleVtyEvent ArrowUp = modify selectNextItem
handleVtyEvent ArrowRight = modifyFsIO selectNextValue
handleVtyEvent ArrowLeft = modifyFsIO selectPreviousValue
handleVtyEvent _ = continue

-- | Modify an AppState in response to an Event when said modification
--   involves an IO action.
modifyFsIO :: (AppState -> FsReadIO AppState) -> NextAppState
modifyFsIO func = get >>= liftIO . runFsReadIO . func >>= put

continue :: NextAppState
continue = return ()