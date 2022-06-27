module Util (mapSelected) where

import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import State (Item)

mapSelected :: (Item -> Item) -> NonEmptyCursor Item -> NonEmptyCursor Item
mapSelected fn cursor = cursor