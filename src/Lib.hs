module Lib (buildInitialState, tcmApp) where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    attrName,
    continue,
    fg,
    halt,
    showFirstCursor,
    str,
    vBox,
    withAttr,
  )
import Brick.Widgets.Border (border)
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor,
    makeNonEmptyCursor,
    nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
    nonEmptyCursorSelectIndex,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
    nonEmptyCursorSelection,
    rebuildNonEmptyCursor,
  )
import Data.List.NonEmpty as NE (nonEmpty, tail)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, splitOn, unpack)
import Graphics.Vty.Attributes (currentAttr, cyan)
import Graphics.Vty.Input.Events (Event (EvKey), Key (KChar, KDown, KUp))
import System.Exit (die)

type Title = Text

type TargetFile = FilePath

type CurrentValue = Text

type PossibleValues = [Text]

data Item = Item Title TargetFile CurrentValue PossibleValues deriving stock (Show, Eq)

data AppState = AppState (NonEmptyCursor Item) deriving stock (Show, Eq)

testConfigFilePath :: FilePath
testConfigFilePath = "test/data/config"

toItem :: [Text] -> Item
toItem chunks = Item title (unpack path) currentValue possibleValues
  where
    title = chunks !! 0
    path = chunks !! 1
    currentValue = chunks !! 2
    possibleValues = drop 3 chunks

separateValues :: Text -> [Text]
separateValues = splitOn ","

buildInitialState :: IO AppState
buildInitialState = do
  configLines <- fmap (map pack . lines) $ readFile testConfigFilePath :: IO [Text]
  let a = map separateValues configLines
      items :: [Item]
      items = map toItem a
   in case NE.nonEmpty items of
        Nothing -> die "There are no directory contents to show."
        Just ne -> pure $ AppState (makeNonEmptyCursor ne)

tcmApp :: App AppState e ResourceName
tcmApp =
  App
    { appDraw = drawTCM,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap currentAttr [(attrName "selected", fg cyan)]
    }

data ResourceName
  = ResourceName
  deriving stock (Show, Eq, Ord)

drawTCM :: AppState -> [Widget ResourceName]
drawTCM (AppState items) =
  [ vBox $
      concat
        [ map (drawPath False) $ reverse $ nonEmptyCursorPrev items,
          [border $ drawPath True $ nonEmptyCursorCurrent items],
          map (drawPath False) $ nonEmptyCursorNext items
        ]
  ]

drawPath :: Bool -> Item -> Widget ResourceName
drawPath isHighlighted (Item title targetFile currentValue possibleValues) = (attachAttrWhenHighlighted isHighlighted . str . unpack) title

attachAttrWhenHighlighted :: Bool -> Widget n -> Widget n
attachAttrWhenHighlighted isHighlighted
  | isHighlighted = withAttr $ attrName "selected"
  | otherwise = id

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
        _ -> continue (AppState items)
  | otherwise = continue (AppState items)

deleteFirstEntry :: AppState -> AppState
deleteFirstEntry (AppState items) = (AppState . tailOfNonEmptyCursor) items
  where
    tailOfNonEmptyCursor :: NonEmptyCursor a -> NonEmptyCursor a
    tailOfNonEmptyCursor = fromJust . nonEmptyCursorSelectIndex selectionPosition . makeNonEmptyCursor . fromJust . NE.nonEmpty . NE.tail . rebuildNonEmptyCursor -- FIXME handle Maybe
    selectionPosition = max 0 $ nonEmptyCursorSelection items - 1 -- FIXME handle Maybe