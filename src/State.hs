module State
  ( AppState (MkAppState),
    NextAppState,
    ResourceName,
  )
where

import Brick
  ( EventM,
    Next,
  )
import Config
  ( ConfigItem,
  )
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
  )

data ResourceName = MkResourceName deriving stock (Show, Eq, Ord)

data AppState = MkAppState (NonEmptyCursor ConfigItem) deriving stock (Show, Eq)

type NextAppState = EventM ResourceName (Next AppState)