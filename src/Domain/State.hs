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