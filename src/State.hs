module State
  ( AppState (MkAppState),
    ResourceName,
  )
where

import Config (ConfigItem)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)

data ResourceName = MkResourceName deriving stock (Show, Eq, Ord)

data AppState = MkAppState (NonEmptyCursor ConfigItem) deriving stock (Show, Eq)