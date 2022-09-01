-- |
-- Module      : ValueSelection
-- Description : Expose functions to change the application state during value
--   selection.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.ValueSelection
  ( selectNextValue,
    selectPreviousValue,
    elementAfter,
    elementBefore,
    changeNthElement,
    valueMarker,
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
    nonEmptyCursorSelection,
    rebuildNonEmptyCursor,
  )
import Data.List.NonEmpty
  ( NonEmpty,
    fromList,
    toList,
  )
import Data.Maybe
  ( fromMaybe,
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

-- | When applied to an application state, select the next value of the item
--   under the cursor and modify the corresponding file accordingly.
--   TODO AppState as input type
selectNextValue :: NonEmptyCursor ConfigItem -> NextAppState
selectNextValue = selectValueAndModifyTargetFile (cycleCurrentValue (elementAfter))

-- | When applied to an application state, select the previous value of the item
--   under the cursor and modify the corresponding file accordingly.
selectPreviousValue :: NonEmptyCursor ConfigItem -> NextAppState
selectPreviousValue = selectValueAndModifyTargetFile (cycleCurrentValue (elementBefore))

-- | Depending on the supplied policy, either switch the value to the next or previous possible value.
--   A switched value is written back to the configured file at the place identified by the pattern.
--   Caution! Reading the file needs to be strict here in order to not have file handles open for subsequent
--   modifications.
selectValueAndModifyTargetFile :: (AppState -> AppState) -> NonEmptyCursor ConfigItem -> NextAppState
selectValueAndModifyTargetFile selectionPolicy items =
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

type ValueCyclingPolicy = TargetValue -> [TargetValue] -> TargetValue

-- | Produce a new AppState where the value of the currently selected item is switched to a new one according to the
--   supplied policy
cycleCurrentValue :: ValueCyclingPolicy -> AppState -> AppState
cycleCurrentValue policy (MkAppState items) = (MkAppState . changeElementUnderCursor (cycleItemValue policy)) items

type CursorPosition = Int

-- | Sets the cursor to a given position.
restoreCursor :: CursorPosition -> NonEmptyCursor a -> NonEmptyCursor a
restoreCursor cursorPosition nec = fromMaybe nec (nonEmptyCursorSelectIndex cursorPosition nec)

-- | Switch the active target value to another value contained in possibleValues according to the supplied policy.
cycleItemValue :: ValueCyclingPolicy -> ConfigItem -> ConfigItem
cycleItemValue policy item = item {targetValue = policy (targetValue item) (possibleValues item)}

-- | A mode of selecting a new element from a list relative to another element.
data NeighborSelection = SelectSuccessor | SelectPredecessor

-- | Finds the first element equal to the input element inside of a list and
--   returns an element next to it depending on the supplied selection policy.
elementNextTo :: Eq t => NeighborSelection -> t -> [t] -> t
elementNextTo neighborSelection targetItem list
  | null list = targetItem
  | not (elem targetItem list) = head list
  | SelectSuccessor <- neighborSelection = chooseNextWrapping targetItem list
  | SelectPredecessor <- neighborSelection = chooseNextWrapping targetItem (reverse list)

chooseNextWrapping :: Eq t => t -> [t] -> t
chooseNextWrapping targetItem = (!! 1) . dropWhile (/= targetItem) . concat . replicate 2

-- | Finds the first element equal to the input element inside of a list and
--   returns the element after it (one index up).
elementAfter :: Eq t => t -> [t] -> t
elementAfter = elementNextTo SelectSuccessor

-- | Finds the first element equal to the input element inside of a list and
--   returns the element before it (one index down).
elementBefore :: Eq t => t -> [t] -> t
elementBefore = elementNextTo SelectPredecessor

-- | Apply a function to the element under the cursor.
changeElementUnderCursor :: (a -> a) -> NonEmptyCursor a -> NonEmptyCursor a
changeElementUnderCursor fn nec =
  ( restoreCursor cursorPosition
      . makeNonEmptyCursor
      . changeNthElementNonEmpty cursorPosition fn
      . rebuildNonEmptyCursor
  )
    nec
  where
    cursorPosition = nonEmptyCursorSelection nec

-- | Given an index, a function and a NonEmpty list it applies the function to
--   the element at that index.
changeNthElementNonEmpty :: Int -> (t -> t) -> NonEmpty t -> NonEmpty t
changeNthElementNonEmpty n fn = fromList . changeNthElement n fn . toList

-- | Given an index, a function and a list it applies the function to the
--   element at that index.
changeNthElement :: Int -> (t -> t) -> [t] -> [t]
changeNthElement _ _ [] = []
changeNthElement n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement (n - 1) fn xs)