module App (buildInitialState, tcmApp, AppState) where

import Brick
  ( App (..),
    attrMap,
    attrName,
    showFirstCursor,
  )
import Config (Config (Config))
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Graphics.Vty.Attributes
  ( currentAttr,
  )
import Keys (handleEvent)
import Render (drawApp, selectionStyling)
import State
  ( AppState (AppState),
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
      appAttrMap = const $ attrMap currentAttr [(attrName "selected", selectionStyling)]
    }

buildInitialState :: Config -> IO AppState
buildInitialState (Config configItems) = do
  case NE.nonEmpty configItems of
    Nothing -> die errorMsgNoConfigEntries
    Just ne -> pure $ AppState (makeNonEmptyCursor ne)
