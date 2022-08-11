module StateTransition
  ( selectNextItem,
    selectNextValue,
    selectPreviousItem,
    selectPreviousValue,
  )
where

import Brick
  ( EventM,
    Next,
    continue,
  )
import Config
  ( ConfigItem (..),
    Pattern (..),
    TargetValue (..),
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
import FileModification
  ( Content (..),
    modifyFile,
  )
import State
  ( AppState (..),
    NextAppState,
  )
import Util
  ( changeNthElementNonEmpty,
    elementAfter,
    elementBefore,
  )

data ValueSelectionPolicy = MkValueSelectionPolicy (AppState -> AppState)

data ItemSelectionPolicy = MkItemSelectionPolicy (NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem))

selectNextItem :: NonEmptyCursor ConfigItem -> NextAppState
selectNextItem = select $ MkItemSelectionPolicy nonEmptyCursorSelectNext

selectPreviousItem :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousItem = select $ MkItemSelectionPolicy nonEmptyCursorSelectPrev

data ValueCyclingPolicy = MkValueCyclingPolicy (TargetValue -> [TargetValue] -> TargetValue)

selectNextValue :: NonEmptyCursor ConfigItem -> NextAppState
selectNextValue items = selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementAfter))) items

selectPreviousValue :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousValue items = selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementBefore))) items

-- | Depending on the supplied policy, either switch the value to the next or previous possible value.
--   A switched value is written back to the configured file at the place identified by the pattern.
--   Caution! Reading the file needs to be strict here in order to not have file handles open for subsequent
--   modifications.
selectValueAndModifyTargetFile :: ValueSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
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
select :: ItemSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
select (MkItemSelectionPolicy selectionPolicy) items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> (continue . MkAppState) nonEmptyCursor'

valueMarker :: Text
valueMarker = "{{value}}"

-- | Substitute the newly selected value into the old content of the target file. Both the previous value
--   and the pattern are needed to choose the correct substitution.
modify :: TargetValue -> TargetValue -> Pattern -> Content -> Content
modify (MkTargetValue oldValue) (MkTargetValue newValue) (MkPattern matchingPattern) (MkContent content) =
  MkContent (replace oldSubstring newSubstring content)
  where
    oldSubstring = replace valueMarker oldValue matchingPattern
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