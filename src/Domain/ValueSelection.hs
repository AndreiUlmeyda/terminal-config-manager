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
    valueMarker,
  )
where

import Data.Text
  ( Text,
    replace,
  )
import Domain.ItemsCursor
  ( changeElementUnderCursor,
    itemUnderCursor,
  )
import Domain.State
  ( AppState (..),
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
import Infrastructure.FsReadIO (FsReadIO)

-- | When applied to an application state, select the next value of the item
--   under the cursor and modify the corresponding file accordingly.
selectNextValue :: AppState -> FsReadIO AppState
selectNextValue = selectValueAndModifyTargetFile (cycleCurrentValue elementAfter)

-- | When applied to an application state, select the previous value of the item
--   under the cursor and modify the corresponding file accordingly.
selectPreviousValue :: AppState -> FsReadIO AppState
selectPreviousValue = selectValueAndModifyTargetFile (cycleCurrentValue elementBefore)

-- | Depending on the supplied policy, either switch the value to the next or previous possible value.
--   A switched value is written back to the configured file at the place identified by the pattern.
--   Caution! Reading the file needs to be strict here in order to not have file handles open for subsequent
--   modifications.
selectValueAndModifyTargetFile :: (AppState -> AppState) -> AppState -> FsReadIO AppState
selectValueAndModifyTargetFile selectionPolicy (MkAppState itemsCursor) =
  let (MkAppState newItemsCursor) = selectionPolicy (MkAppState itemsCursor)
      currentItem = itemUnderCursor newItemsCursor
      currentValue = targetValue currentItem
      currentPath = path currentItem
      currentPattern = matchingPattern currentItem
      previousItem = itemUnderCursor itemsCursor
      previousValue = targetValue previousItem
      modification = modify previousValue currentValue currentPattern
   in modifyFile currentPath modification >> return (MkAppState newItemsCursor)

-- | This needs to be a substring of the matching pattern and is needed
--   to determine the location of the value which is to be replaced.
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
  | targetItem `notElem` list = head list
  | SelectSuccessor <- neighborSelection = chooseNextWhileWrapping targetItem list
  | SelectPredecessor <- neighborSelection = chooseNextWhileWrapping targetItem (reverse list)

chooseNextWhileWrapping :: Eq t => t -> [t] -> t
chooseNextWhileWrapping targetItem [] = targetItem
chooseNextWhileWrapping targetItem list = (head . tail . dropWhile (/= targetItem) . concat . replicate 2) list

-- | Finds the first element equal to the input element inside of a list and
--   returns the element after it (one index up).
elementAfter :: Eq t => t -> [t] -> t
elementAfter = elementNextTo SelectSuccessor

-- | Finds the first element equal to the input element inside of a list and
--   returns the element before it (one index down).
elementBefore :: Eq t => t -> [t] -> t
elementBefore = elementNextTo SelectPredecessor