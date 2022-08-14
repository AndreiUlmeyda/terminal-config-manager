-- |
-- Module      : Main
-- Description : Type definitions concerning the application state.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.State
  ( AppState (MkAppState),
    NextAppState,
    ResourceName,
  )
where

import Brick
  ( EventM,
    Next,
  )
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
  )
import Infrastructure.Config
  ( ConfigItem,
  )

data ResourceName = MkResourceName deriving stock (Show, Eq, Ord)

data AppState = MkAppState (NonEmptyCursor ConfigItem) deriving stock (Show, Eq)

type NextAppState = EventM ResourceName (Next AppState)