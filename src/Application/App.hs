module Application.App (buildInitialState, tcmApp, AppState, runApp) where

import Brick (App (..), attrMap, attrName, defaultMain, showFirstCursor)
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Domain.State
  ( AppState (MkAppState),
    ResourceName,
  )
import Graphics.Vty.Attributes
  ( currentAttr,
  )
import Infrastructure.Config (Config (MkConfig), loadConfig)
import System.Exit (die, exitSuccess)
import UserInterface.Input (handleEvent)
import UserInterface.Render
  ( drawTCM,
    selectionStyling,
    valueStyling,
  )

runApp :: IO ()
runApp = loadConfig >>= buildInitialState >>= defaultMain tcmApp >>= const exitSuccess

errorMsgNoConfigEntries :: String
errorMsgNoConfigEntries = "There are no entries in the config file."

-- | For this application only event handling, drawing and some attributes need to be implemented. The rest are default
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

-- | Wrap the config file entries in a nonempty list.
buildInitialState :: Config -> IO AppState
buildInitialState (MkConfig configItems) =
  case NE.nonEmpty configItems of
    Nothing -> die errorMsgNoConfigEntries
    Just ne -> (pure . MkAppState . makeNonEmptyCursor) ne
