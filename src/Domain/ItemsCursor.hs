module Domain.ItemsCursor
  ( ItemsCursor (MkItemsCursor),
    makeItemsCursor,
    cursorUp,
  )
where

import Infrastructure.Config (ConfigItem)

type CursorPosition = Int

data ItemsCursor = MkItemsCursor [ConfigItem] CursorPosition deriving stock (Show, Eq)

makeItemsCursor :: [ConfigItem] -> Maybe ItemsCursor
makeItemsCursor [] = Nothing
makeItemsCursor items = Just (MkItemsCursor items 0)

cursorUp :: ItemsCursor -> ItemsCursor
cursorUp (MkItemsCursor items cp) = MkItemsCursor items (cp + 1)