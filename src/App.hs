module App (buildInitialState, tcmApp, AppState) where

import Brick
  ( App (..),
    attrMap,
    attrName,
    showFirstCursor,
  )
import Config (Config (MkConfig))
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Graphics.Vty.Attributes
  ( currentAttr,
  )
import Input (handleEvent)
import Render (drawTCM, selectionStyling, valueStyling)
import State
  ( AppState (MkAppState),
    ResourceName,
  )
import System.Exit (die)

errorMsgNoConfigEntries :: String
errorMsgNoConfigEntries = "There are no entries in the config file."

-- For this application only event handling, drawing and some attributes need to be implemented. The rest are default
-- implementations.
tcmApp :: App AppState e ResourceName
tcmApp =
  App
    { appHandleEvent = handleEvent,
      appDraw = drawTCM,
      appAttrMap = (const . attrMap currentAttr) [(attrName "selected", selectionStyling), (attrName "value", valueStyling)],
      appChooseCursor = showFirstCursor,
      appStartEvent = pure
    }

-- Wrap the config file entries in a nonempty list.
buildInitialState :: Config -> IO AppState
buildInitialState (MkConfig configItems) =
  case NE.nonEmpty configItems of
    Nothing -> die errorMsgNoConfigEntries
    Just ne -> (pure . MkAppState . makeNonEmptyCursor) ne
