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
handleEvent (MkAppState items) e
  | VtyEvent vtye <- e =
      case vtye of
        EvKey (KChar 'q') [] -> halt (MkAppState items)
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext items of
            Nothing -> continue (MkAppState items)
            Just nonEmptyCursor' -> continue $ MkAppState nonEmptyCursor'
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev items of
            Nothing -> continue (MkAppState items)
            Just nonEmptyCursor' -> continue $ MkAppState nonEmptyCursor'
        EvKey KRight [] ->
          let (MkAppState newItems) = cycleValuesForward (MkAppState items)
              currentItem = nonEmptyCursorCurrent newItems
              previousItem = nonEmptyCursorCurrent items
           in do
                oldContent <- liftIO $ readFile (path currentItem)
                let newContent = modify (value previousItem) (value currentItem) (pattern currentItem) (pack oldContent)
                 in liftIO $ writeFile (path currentItem) (unpack newContent)
                continue (MkAppState newItems)
        _ -> continue (MkAppState items)
  | otherwise = continue (MkAppState items)

modify :: Text -> Text -> Text -> Text -> Text
modify oldValue newValue pattern content = replace oldSubstring newSubstring content
  where
    oldSubstring = replace "{{value}}" oldValue pattern
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

-- TODO write state back to config file