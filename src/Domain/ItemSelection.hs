-- |
-- Module      : ItemSelection
-- Description : Expose functions to change the application state during item
--   selection.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.ItemSelection
  ( selectNextItem,
    selectPreviousItem,
  )
where

import Brick
  ( continue,
  )
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
  )
import Domain.State
  ( AppState (MkAppState),
    NextAppState,
  )
import Infrastructure.Config
  ( ConfigItem,
  )

-- | A function type which, when applied to an application state, changes the
--   cursor position.
data ItemSelectionPolicy = MkItemSelectionPolicy (NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem))

-- | A function to change the cursor position to point at the next item.
selectNextItem :: NonEmptyCursor ConfigItem -> NextAppState
selectNextItem = selectItem $ MkItemSelectionPolicy nonEmptyCursorSelectNext

-- | A function to change the cursor position to point at the previous item.
selectPreviousItem :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousItem = selectItem $ MkItemSelectionPolicy nonEmptyCursorSelectPrev

-- | Depending on the supplied policy, either select the next or previous item
selectItem :: ItemSelectionPolicy -> NonEmptyCursor ConfigItem -> NextAppState
selectItem (MkItemSelectionPolicy selectionPolicy) items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> (continue . MkAppState) nonEmptyCursor'