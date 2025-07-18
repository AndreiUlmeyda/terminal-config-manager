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

import Domain.ItemsCursor
  ( cursorDown,
    cursorUp,
  )
import Domain.State
  ( AppState (MkAppState),
  )

-- | A function to change the cursor position to point at the next item.
selectNextItem :: AppState -> AppState
selectNextItem (MkAppState itemsCursor) = MkAppState (cursorUp itemsCursor)

-- | A function to change the cursor position to point at the previous item.
selectPreviousItem :: AppState -> AppState
selectPreviousItem (MkAppState itemsCursor) = MkAppState (cursorDown itemsCursor)
