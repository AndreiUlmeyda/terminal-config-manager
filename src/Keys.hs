module Keys (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    continue,
    halt,
  )
import Config (ConfigItem (..))
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    makeNonEmptyCursor,
    nonEmptyCursorSelectIndex,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelection,
    rebuildNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( NonEmpty,
    fromList,
    nonEmpty,
    tail,
    toList,
  )
import Data.Maybe
  ( fromJust,
  )
import Graphics.Vty.Input.Events
  ( Event (EvKey),
    Key (KChar, KDown, KLeft, KRight, KUp),
  )
import State (AppState (MkAppState))

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent (MkAppState items) e
  | VtyEvent vtye <- e =
      case vtye of
        EvKey (KChar 'q') [] -> halt (MkAppState items)
        EvKey (KChar 'd') [] -> continue $ deleteFirstEntry (MkAppState items)
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext items of
            Nothing -> continue (MkAppState items)
            Just nonEmptyCursor' -> continue $ MkAppState nonEmptyCursor'
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev items of
            Nothing -> continue (MkAppState items)
            Just nonEmptyCursor' -> continue $ MkAppState nonEmptyCursor'
        EvKey KRight [] -> continue $ cycleValuesForward (MkAppState items)
        EvKey KLeft [] -> continue $ cycleValuesBackward (MkAppState items)
        _ -> continue (MkAppState items)
  | otherwise = continue (MkAppState items)

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
cycleForward' (MkConfigItem title targetFile currentValue possibleValues) = (MkConfigItem title targetFile nextValue possibleValues)
  where
    nextValue = (head . Prelude.tail . dropWhile (/= currentValue) . cycle) possibleValues

cycleValuesBackward :: AppState -> AppState
cycleValuesBackward (MkAppState items) = (MkAppState . cycleBackward) items
  where
    cycleBackward :: NonEmptyCursor a -> NonEmptyCursor a
    cycleBackward = id

deleteFirstEntry :: AppState -> AppState -- define a "mapSelected", express all item operations using it
deleteFirstEntry (MkAppState items) = (MkAppState . tailOfNonEmptyCursor) items
  where
    tailOfNonEmptyCursor :: NonEmptyCursor a -> NonEmptyCursor a
    tailOfNonEmptyCursor = fromJust . nonEmptyCursorSelectIndex selectionPosition . makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor -- FIXME handle Maybe
    selectionPosition = max 0 $ nonEmptyCursorSelection items - 1 :: Int

changeNthElement :: Int -> (a -> a) -> NonEmpty a -> NonEmpty a
changeNthElement n fn = fromList . changeNthElement' n fn . toList

changeNthElement' :: Int -> (a -> a) -> [a] -> [a]
changeNthElement' _ _ [] = []
changeNthElement' n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement' (n - 1) fn xs)