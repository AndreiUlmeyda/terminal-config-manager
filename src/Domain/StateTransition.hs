-- |
-- Module      : StateTransition
-- Description : Expose functions to change the application state. This covers
--    item and value selection.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.StateTransition
  ( selectNextItem,
    selectNextValue,
    selectPreviousItem,
    selectPreviousValue,
  )
where

import Brick
  ( continue,
  )
import Control.Monad.IO.Class
  ( MonadIO (liftIO),
  )
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    makeNonEmptyCursor,
    nonEmptyCursorCurrent,
    nonEmptyCursorSelectIndex,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelection,
    rebuildNonEmptyCursor,
  )
import Data.Text
  ( Text,
    replace,
  )
import Domain.State
  ( AppState (..),
    NextAppState,
  )
import Infrastructure.Config
  ( ConfigItem (..),
    Pattern (..),
    TargetValue (..),
  )
import Infrastructure.FileModification
  ( Content (..),
    modifyFile,
  )
import Infrastructure.Util
  ( changeNthElementNonEmpty,
    elementAfter,
    elementBefore,
  )

-- | A function type which, when applied to an application state, changes the
--   value of the item under the cursor.
data ValueSelectionPolicy = MkValueSelectionPolicy (AppState -> AppState)

-- | A function type which, when applied to an application state, changes the
--   cursor position.
data ItemSelectionPolicy = MkItemSelectionPolicy (NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem))

-- | A function to change the cursor position to point at the next item.
--   TODO AppState as input type
selectNextItem :: NonEmptyCursor ConfigItem -> NextAppState
selectNextItem = selectItem $ MkItemSelectionPolicy nonEmptyCursorSelectNext

-- | A function to change the cursor position to point at the previous item.
selectPreviousItem :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousItem = selectItem $ MkItemSelectionPolicy nonEmptyCursorSelectPrev

-- | A function type which, given a value and a list of possible values,
--   generates a new value.
data ValueCyclingPolicy = MkValueCyclingPolicy (TargetValue -> [TargetValue] -> TargetValue)

-- | When applied to an application state, select the next value of the item
--   under the cursor and modify the corresponding file accordingly.
--   TODO AppState as input type
selectNextValue :: NonEmptyCursor ConfigItem -> NextAppState
selectNextValue items = selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementAfter))) items

-- | When applied to an application state, select the previous value of the item
--   under the cursor and modify the corresponding file accordingly.
selectPreviousValue :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousValue items = selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementBefore))) items

-- | Depending on the supplied policy, either switch the value to the next or previous possible value.
--   A switched value is written back to the configured file at the place identified by the pattern.
--   Caution! Reading the file needs to be strict here in order to not have file handles open for subsequent
--   modifications.
selectValueAndModifyTargetFile :: ValueSelectionPolicy -> NonEmptyCursor ConfigItem -> NextAppState
selectValueAndModifyTargetFile (MkValueSelectionPolicy selectionPolicy) items =
  let (MkAppState newItems) = selectionPolicy (MkAppState items)
      currentItem = nonEmptyCursorCurrent newItems
      currentValue = targetValue currentItem
      currentPath = path currentItem
      currentPattern = matchingPattern currentItem
      previousItem = nonEmptyCursorCurrent items
      previousValue = targetValue previousItem
      modification = modify previousValue currentValue currentPattern
   in do
        _ <- liftIO $ modifyFile currentPath modification
        continue (MkAppState newItems)

-- | Depending on the supplied policy, either select the next or previous item
selectItem :: ItemSelectionPolicy -> NonEmptyCursor ConfigItem -> NextAppState
selectItem (MkItemSelectionPolicy selectionPolicy) items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> (continue . MkAppState) nonEmptyCursor'

-- | This needs to be a substring of the matching pattern and is needed
--   to determine the location of the values to replaced.
valueMarker :: Text
valueMarker = "{{value}}"

-- | Substitute the newly selected value into the old content of the target file. Both the previous value
--   and the pattern are needed to choose the correct substitution.
modify :: TargetValue -> TargetValue -> Pattern -> (Content -> Content)
modify (MkTargetValue oldValue) (MkTargetValue newValue) (MkPattern matchPattern) (MkContent content) =
  MkContent (replace oldSubstring newSubstring content)
  where
    oldSubstring = replace valueMarker oldValue matchPattern
    newSubstring = replace oldValue newValue oldSubstring

-- | Produce a new AppState where the value of the currently selected item is switched to a new one according to the
--   supplied policy
cycleValues :: ValueCyclingPolicy -> AppState -> AppState
cycleValues policy (MkAppState items) = (MkAppState . cycleSelected) items
  where
    restorePosition :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
    restorePosition = nonEmptyCursorSelectIndex selectionPosition
    cycledSelected = (makeNonEmptyCursor . changeNthElementNonEmpty selectionPosition (cycleTo policy) . rebuildNonEmptyCursor) items
    cycleSelected :: NonEmptyCursor ConfigItem -> NonEmptyCursor ConfigItem
    cycleSelected = case restorePosition cycledSelected of
      Nothing -> const cycledSelected
      Just restored -> const restored
    selectionPosition = nonEmptyCursorSelection items

-- | Switch the active target value to another value contained in possibleValues according to the supplied policy.
cycleTo :: ValueCyclingPolicy -> ConfigItem -> ConfigItem
cycleTo (MkValueCyclingPolicy policy) item = item {targetValue = policy (targetValue item) (possibleValues item)}