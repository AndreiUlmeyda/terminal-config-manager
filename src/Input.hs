module Input (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    continue,
    halt,
  )
import Config (ConfigItem (..), Value (..))
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
import Data.Text (Text, pack, replace, unpack)
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )
import State (AppState (MkAppState))
import System.IO.Strict (readFile)
import Util
  ( changeNthElementNonEmpty,
    elementAfter,
    elementBefore,
  )
import Prelude hiding (readFile)

-- | Handle an event emitted by brick by unpacking the underlying vty event and passing it the appropriate handler.
handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

-- | Handle a keyboard event, up and down keys for selection, left and right for changing the associated value and q to quit.
handleVtyEvent :: Event -> AppState -> EventM n (Next AppState)
handleVtyEvent event (MkAppState items) = case event of
  EvKey (KChar 'q') [] -> halt (MkAppState items)
  EvKey KDown [] -> select next items
  EvKey KUp [] -> select previous items
  EvKey KRight [] -> selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementAfter))) items
  EvKey KLeft [] -> selectValueAndModifyTargetFile (MkValueSelectionPolicy (cycleValues (MkValueCyclingPolicy elementBefore))) items
  _ -> continue (MkAppState items)

data ValueSelectionPolicy = MkValueSelectionPolicy (AppState -> AppState)

-- | Depending on the supplied policy, either switch the value to the next or previous possible value.
--   A switched value is written back to the configured file at the place identified by the pattern.
selectValueAndModifyTargetFile :: ValueSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
selectValueAndModifyTargetFile (MkValueSelectionPolicy selectionPolicy) items =
  let (MkAppState newItems) = selectionPolicy (MkAppState items)
      currentItem = nonEmptyCursorCurrent newItems
      currentValue = value currentItem
      currentPath = path currentItem
      currentPattern = pattern currentItem
      previousItem = nonEmptyCursorCurrent items
      previousValue = value previousItem
   in do
        oldContent <- (liftIO . readFile) currentPath
        let (MkContent newContent) = modify previousValue currentValue (MkPattern currentPattern) (MkContent (pack oldContent))
         in (liftIO . writeFile currentPath) (unpack newContent)
        continue (MkAppState newItems)

data ItemSelectionPolicy = MkItemSelectionPolicy (NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem))

-- | Depending on the supplied policy, either select the next or previous item
select :: ItemSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
select (MkItemSelectionPolicy selectionPolicy) items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> (continue . MkAppState) nonEmptyCursor'

next :: ItemSelectionPolicy
next = MkItemSelectionPolicy nonEmptyCursorSelectNext

previous :: ItemSelectionPolicy
previous = MkItemSelectionPolicy nonEmptyCursorSelectPrev

valueMarker :: Text
valueMarker = "{{value}}"

data Pattern = MkPattern Text

data Content = MkContent Text

modify :: Value -> Value -> Pattern -> Content -> Content
modify (MkValue oldValue) (MkValue newValue) (MkPattern pattern) (MkContent content) = MkContent (replace oldSubstring newSubstring content)
  where
    oldSubstring = replace valueMarker oldValue pattern
    newSubstring = replace oldValue newValue oldSubstring

-- TODO revisit the naming of the functions below
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

data ValueCyclingPolicy = MkValueCyclingPolicy (Value -> [Value] -> Value)

cycleTo :: ValueCyclingPolicy -> ConfigItem -> ConfigItem
cycleTo (MkValueCyclingPolicy policy) item = item {value = policy (value item) (possibleValues item)}

-- TODO write state back to config file (on program exit should suffice)
-- TODO move all the logic into a more appropriate module
-- TODO find and handle every operation that can fail
-- TODO handle the case where the target file value and config value dont match
-- TODO supply docblocks