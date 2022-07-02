module Input (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    continue,
    halt,
  )
import Config (ConfigItem (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Data.List.NonEmpty as NE
  ( NonEmpty,
    fromList,
    toList,
  )
import Data.Text (Text, pack, replace, unpack)
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KRight, KUp),
  )
import State (AppState (MkAppState))
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

handleVtyEvent :: Event -> AppState -> EventM n (Next AppState)
handleVtyEvent event (MkAppState items) = case event of
  EvKey (KChar 'q') [] -> halt (MkAppState items)
  EvKey KDown [] -> select next items
  EvKey KUp [] -> select previous items
  EvKey KRight [] -> selectNextValueAndModifyTargetFile items
  _ -> continue (MkAppState items)

selectNextValueAndModifyTargetFile :: NonEmptyCursor ConfigItem -> EventM n (Next AppState)
selectNextValueAndModifyTargetFile items =
  let (MkAppState newItems) = cycleValuesForward (MkAppState items)
      currentItem = nonEmptyCursorCurrent newItems
      currentValue = value currentItem
      currentPath = path currentItem
      currentPattern = pattern currentItem
      previousItem = nonEmptyCursorCurrent items
      previousValue = value previousItem
   in do
        oldContent <- liftIO $ readFile currentPath
        let newContent = modify previousValue currentValue currentPattern (pack oldContent)
         in liftIO $ writeFile currentPath (unpack newContent)
        continue (MkAppState newItems)

select :: (NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem)) -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
select selectionPolicy items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> continue $ MkAppState nonEmptyCursor'

next :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
next = nonEmptyCursorSelectNext

previous :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
previous = nonEmptyCursorSelectPrev

valueMarker :: Text
valueMarker = "{{value}}"

modify :: Text -> Text -> Text -> Text -> Text
modify oldValue newValue pattern content = replace oldSubstring newSubstring content
  where
    oldSubstring = replace valueMarker oldValue pattern
    newSubstring = replace oldValue newValue oldSubstring

cycleValuesForward :: AppState -> AppState
cycleValuesForward (MkAppState items) = (MkAppState . cycleSelected) items
  where
    restorePosition :: NonEmptyCursor a -> Maybe (NonEmptyCursor a)
    restorePosition = nonEmptyCursorSelectIndex selectionPosition
    cycledSelected = (makeNonEmptyCursor . changeNthElement selectionPosition cycleForward' . rebuildNonEmptyCursor) items
    cycleSelected :: NonEmptyCursor ConfigItem -> NonEmptyCursor ConfigItem
    cycleSelected = case restorePosition cycledSelected of
      Nothing -> const cycledSelected
      Just restored -> const restored
    selectionPosition = nonEmptyCursorSelection items

cycleForward' :: ConfigItem -> ConfigItem
cycleForward' (MkConfigItem title targetFile pattern currentValue possibleValues) = (MkConfigItem title targetFile pattern nextValue possibleValues)
  where
    nextValue = (head . Prelude.tail . dropWhile (/= currentValue) . cycle) possibleValues

changeNthElement :: Int -> (a -> a) -> NonEmpty a -> NonEmpty a
changeNthElement n fn = fromList . changeNthElement' n fn . toList

changeNthElement' :: Int -> (a -> a) -> [a] -> [a]
changeNthElement' _ _ [] = []
changeNthElement' n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement' (n - 1) fn xs)

-- TODO chop long functions apart
-- TODO write state back to config file (on program exit should suffice)
-- TODO move all the logic into a more appropriate module
-- TODO how about some tests, eh?
-- TODO find and handle every operation that can fail
-- TODO handle the case where the target file value and config value dont match