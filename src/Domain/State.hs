-- |
-- Module      : State
-- Description : Type definitions concerning the application state.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.State
  ( AppState (..),
    NextAppState,
    ResourceName,
  )
where

import Brick
  ( EventM,
  )
import Domain.ItemsCursor
  ( ItemsCursor,
  )

-- | An dummy type used only as a parameter of EventM
data ResourceName = MkResourceName deriving stock (Show, Eq, Ord)

-- | The state of the application. It will guarantee that the list of items
--   contains at least one element and keep track of a cursor position.
newtype AppState = MkAppState ItemsCursor deriving stock (Show, Eq)

-- | When reacting to an event the result needs to be of type EventM. Though
--   not entirely accurate the synonym here is trying to communicate that, in
--   essence, a new state of the application is created.
type NextAppState = EventM ResourceName AppState ()