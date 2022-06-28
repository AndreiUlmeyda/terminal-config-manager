module State
  ( AppState (AppState),
    ResourceName,
  )
where

import Config (ConfigItem)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

data AppState = AppState (NonEmptyCursor ConfigItem) deriving stock (Show, Eq)