module Keys (handleEvent) where

import Brick
  ( BrickEvent (VtyEvent),
    EventM,
    Next,
    continue,
    halt,
  )
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
import State (AppState (AppState), Item (Item))

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent (AppState items) e
  | VtyEvent vtye <- e =
      case vtye of
        EvKey (KChar 'q') [] -> halt (AppState items)
        EvKey (KChar 'd') [] -> continue $ deleteFirstEntry (AppState items)
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext items of
            Nothing -> continue (AppState items)
            Just nonEmptyCursor' -> continue $ AppState nonEmptyCursor'
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev items of
            Nothing -> continue (AppState items)
            Just nonEmptyCursor' -> continue $ AppState nonEmptyCursor'
        EvKey KRight [] -> continue $ cycleValuesForward (AppState items)
        EvKey KLeft [] -> continue $ cycleValuesBackward (AppState items)
        _ -> continue (AppState items)
  | otherwise = continue (AppState items)

cycleValuesForward :: AppState -> AppState
cycleValuesForward (AppState items) = (AppState . cycleForward) items
  where
    cycleForward :: NonEmptyCursor Item -> NonEmptyCursor Item
    cycleForward = makeNonEmptyCursor . changeNthElement selectionPosition cycleForward' . rebuildNonEmptyCursor
    selectionPosition = nonEmptyCursorSelection items

cycleForward' :: Item -> Item
cycleForward' (Item title targetFile currentValue possibleValues) = (Item title targetFile nextValue possibleValues)
  where
    nextValue = (head . Prelude.tail . dropWhile (/= currentValue) . cycle) possibleValues

cycleValuesBackward :: AppState -> AppState
cycleValuesBackward (AppState items) = (AppState . cycleBackward) items
  where
    cycleBackward :: NonEmptyCursor a -> NonEmptyCursor a
    cycleBackward = id

deleteFirstEntry :: AppState -> AppState
deleteFirstEntry (AppState items) = (AppState . tailOfNonEmptyCursor) items
  where
    tailOfNonEmptyCursor :: NonEmptyCursor a -> NonEmptyCursor a
    tailOfNonEmptyCursor = fromJust . nonEmptyCursorSelectIndex selectionPosition . makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor -- FIXME handle Maybe
    selectionPosition = max 0 $ nonEmptyCursorSelection items - 1 -- FIXME handle Maybe

changeNthElement :: Int -> (a -> a) -> NonEmpty a -> NonEmpty a
changeNthElement n fn = fromList . changeNthElement' n fn . toList

changeNthElement' :: Int -> (a -> a) -> [a] -> [a]
changeNthElement' _ _ [] = []
changeNthElement' n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement' (n - 1) fn xs)