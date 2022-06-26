module State
  ( AppState (AppState),
    Item (Item),
    ConfigValue,
    ResourceName,
  )
where

import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import Data.Text (Text)

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

type ConfigValue = Text

type Title = Text

type TargetFile = FilePath

type CurrentValue = ConfigValue

type PossibleValues = [ConfigValue]

data Item = Item Title TargetFile CurrentValue PossibleValues deriving stock (Show, Eq)

data AppState = AppState (NonEmptyCursor Item) deriving stock (Show, Eq)