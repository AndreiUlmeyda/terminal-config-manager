module Domain.ItemsCursor
  ( ItemsCursor (MkItemsCursor),
    makeItemsCursor,
  )
where

import Infrastructure.Config (ConfigItem)

data ItemsCursor = MkItemsCursor [ConfigItem] deriving stock (Show, Eq)

makeItemsCursor :: [ConfigItem] -> Maybe ItemsCursor
makeItemsCursor [] = Nothing
makeItemsCursor items = Just (MkItemsCursor items)