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

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent currentState event
  | VtyEvent vtye <- event = handleVtyEvent vtye currentState
  | otherwise = continue currentState

handleVtyEvent :: Event -> AppState -> EventM n (Next AppState)
handleVtyEvent event (MkAppState items) = case event of
  EvKey (KChar 'q') [] -> halt (MkAppState items)
  EvKey KDown [] -> select next items
  EvKey KUp [] -> select previous items
  EvKey KRight [] -> selectValueAndModifyTargetFile (cycleValues elementAfter) items
  EvKey KLeft [] -> selectValueAndModifyTargetFile (cycleValues elementBefore) items
  _ -> continue (MkAppState items)

type ValueSelectionPolicy = AppState -> AppState

selectValueAndModifyTargetFile :: ValueSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
selectValueAndModifyTargetFile selectionPolicy items =
  let (MkAppState newItems) = selectionPolicy (MkAppState items)
      currentItem = nonEmptyCursorCurrent newItems
      currentValue = value currentItem
      currentPath = path currentItem
      currentPattern = pattern currentItem
      previousItem = nonEmptyCursorCurrent items
      previousValue = value previousItem
   in do
        oldContent <- (liftIO . readFile) currentPath
        let newContent = modify previousValue currentValue currentPattern (pack oldContent)
         in (liftIO . writeFile currentPath) (unpack newContent)
        continue (MkAppState newItems)

type ItemSelectionPolicy = NonEmptyCursor ConfigItem -> Maybe (NonEmptyCursor ConfigItem)

select :: ItemSelectionPolicy -> NonEmptyCursor ConfigItem -> EventM n (Next AppState)
select selectionPolicy items = case selectionPolicy items of
  Nothing -> continue (MkAppState items)
  Just nonEmptyCursor' -> (continue . MkAppState) nonEmptyCursor'

next :: ItemSelectionPolicy
next = nonEmptyCursorSelectNext

previous :: ItemSelectionPolicy
previous = nonEmptyCursorSelectPrev

valueMarker :: Text
valueMarker = "{{value}}"

type Value = Text

type Pattern = Text

type Content = Text

modify :: Value -> Value -> Pattern -> Content -> Content
modify oldValue newValue pattern content = replace oldSubstring newSubstring content
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

type ValueCyclingPolicy = (Text -> [Text] -> Text)

cycleTo :: ValueCyclingPolicy -> ConfigItem -> ConfigItem
cycleTo policy item = item {value = policy (value item) (possibleValues item)}

-- TODO write state back to config file (on program exit should suffice)
-- TODO move all the logic into a more appropriate module
-- TODO find and handle every operation that can fail
-- TODO handle the case where the target file value and config value dont match
-- TODO supply docblocks