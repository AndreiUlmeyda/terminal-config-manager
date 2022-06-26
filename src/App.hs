module App (buildInitialState, tcmApp, AppState) where

import Brick
  ( App (..),
    attrMap,
    attrName,
    fg,
    showFirstCursor,
  )
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Data.Text
  ( Text,
    pack,
    splitOn,
    unpack,
  )
import Graphics.Vty.Attributes
  ( currentAttr,
    cyan,
  )
import Keys (handleEvent)
import Render (drawApp)
import State
  ( AppState (AppState),
    ConfigValue,
    Item (Item),
    ResourceName,
  )
import System.Exit (die)

errorMsgNoConfigEntries :: String
errorMsgNoConfigEntries = "There are no entries in the config file."

tcmApp :: App AppState e ResourceName
tcmApp =
  App
    { appDraw = drawApp,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap currentAttr [(attrName "selected", fg cyan)]
    }

testConfigFilePath :: FilePath
testConfigFilePath = "test/data/config"

buildInitialState :: IO AppState
buildInitialState = do
  configLines <- fmap (map pack . lines) $ readFile testConfigFilePath :: IO [Text]
  let configValues = map separateValues configLines :: [[ConfigValue]]
      items = map toItem configValues :: [Item]
   in case NE.nonEmpty items of
        Nothing -> die errorMsgNoConfigEntries
        Just ne -> pure $ AppState (makeNonEmptyCursor ne)

toItem :: [Text] -> Item
toItem chunks = Item title (unpack path) currentValue possibleValues
  where
    title = chunks !! 0
    path = chunks !! 1
    currentValue = chunks !! 3
    possibleValues = drop 3 chunks

separateValues :: Text -> [Text]
separateValues = splitOn ","
