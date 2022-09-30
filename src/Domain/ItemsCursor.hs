-- |
-- Module      : ItemsCursor
-- Description : Provide a type representing a non empty list of ConfigItems as
--   well as functions to manipulate the cursor and items located at the cursor
--   position.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.ItemsCursor
  ( ItemsCursor (..),
    makeItemsCursor,
    cursorUp,
    cursorDown,
    changeElementUnderCursor,
    itemUnderCursor,
    itemsAfterCursor,
    itemsBeforeCursor,
    changeNthElement,
  )
where

import Infrastructure.Config (ConfigItem)

type CursorPosition = Int

-- | The type representing a non-empty list of ConfigItems in addition to a
--   cursor.
data ItemsCursor = MkItemsCursor
  { items :: [ConfigItem],
    cursorPosition :: CursorPosition
  }
  deriving stock (Show, Eq)

defaultCursorPosition :: Int
defaultCursorPosition = 0

-- | Construct an ItemsCursor from a list of ConfigItems if it is not empty.
makeItemsCursor :: [ConfigItem] -> Maybe ItemsCursor
makeItemsCursor [] = Nothing
makeItemsCursor its = Just (MkItemsCursor its defaultCursorPosition)

-- | Return the ConfigItem at the cursor position.
itemUnderCursor :: ItemsCursor -> ConfigItem
itemUnderCursor (MkItemsCursor its cp) = its !! cp

-- | Return the list of ConfigItems before the cursor position.
itemsBeforeCursor :: ItemsCursor -> [ConfigItem]
itemsBeforeCursor (MkItemsCursor its cp) = take cp its

-- | Return the list of ConfigItems after the cursor position.
itemsAfterCursor :: ItemsCursor -> [ConfigItem]
itemsAfterCursor (MkItemsCursor its cp) = drop (cp + 1) its

-- | Increment the cursor position without exceeding the lower bound.
--   Incrementing selects items further down the rendered list.
cursorDown :: ItemsCursor -> ItemsCursor
cursorDown (MkItemsCursor its cp) = MkItemsCursor its (min (cp + 1) (length its - 1))

-- | Decrement the cursor position without exceeding the upper bound.
--   Decrementing selects items further up the rendered list.
cursorUp :: ItemsCursor -> ItemsCursor
cursorUp (MkItemsCursor its cp) = MkItemsCursor its (max (cp - 1) 0)

-- Apply a function to the item under the cursor.
changeElementUnderCursor :: (ConfigItem -> ConfigItem) -> ItemsCursor -> ItemsCursor
changeElementUnderCursor fn (MkItemsCursor its cp) = MkItemsCursor (changeNthElement cp fn its) cp

-- | Given an index, a function and a list it applies the function to the
--   element at that index.
changeNthElement :: Int -> (t -> t) -> [t] -> [t]
changeNthElement _ _ [] = []
changeNthElement n fn (x : xs)
  | n < 0 = x : xs
  | n == 0 = fn x : xs
  | otherwise = x : changeNthElement (n - 1) fn xs