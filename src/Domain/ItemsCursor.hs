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

data ItemsCursor = MkItemsCursor
  { items :: [ConfigItem],
    cursorPosition :: CursorPosition
  }
  deriving stock (Show, Eq)

makeItemsCursor :: [ConfigItem] -> Maybe ItemsCursor
makeItemsCursor [] = Nothing
makeItemsCursor items = Just (MkItemsCursor items 0)

itemUnderCursor :: ItemsCursor -> ConfigItem
itemUnderCursor (MkItemsCursor items cursorPosition) = items !! cursorPosition

itemsBeforeCursor :: ItemsCursor -> [ConfigItem]
itemsBeforeCursor (MkItemsCursor items cursorPosition) = take cursorPosition items

itemsAfterCursor :: ItemsCursor -> [ConfigItem]
itemsAfterCursor (MkItemsCursor items cursorPosition) = drop (cursorPosition + 1) items

cursorDown :: ItemsCursor -> ItemsCursor
cursorDown (MkItemsCursor items cp) = MkItemsCursor items (min (cp + 1) (length items - 1))

cursorUp :: ItemsCursor -> ItemsCursor
cursorUp (MkItemsCursor items cp) = MkItemsCursor items (max (cp - 1) 0)

changeElementUnderCursor :: (ConfigItem -> ConfigItem) -> ItemsCursor -> ItemsCursor
changeElementUnderCursor fn (MkItemsCursor items cursorPosition) = MkItemsCursor (changeNthElement cursorPosition fn items) cursorPosition

-- | Given an index, a function and a list it applies the function to the
--   element at that index.
changeNthElement :: Int -> (t -> t) -> [t] -> [t]
changeNthElement _ _ [] = []
changeNthElement n fn (x : xs)
  | n < 0 = x : xs
  | n == 0 = fn x : xs
  | otherwise = x : changeNthElement (n - 1) fn xs