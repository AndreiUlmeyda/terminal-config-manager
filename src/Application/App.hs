-- |
-- Module      : App
-- Description : Build and run a Brick app after loading the config file.
--   Additionally, provide a program description and help text when using the
--   flag --help.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Application.App (buildInitialState, tcmApp, AppState, runApp) where

import Brick (App (..), attrMap, attrName, defaultMain, showFirstCursor)
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Domain.FileSynchronization
  ( synchronizeWithTargetFiles,
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
import UserInterface.Cli (provideHelpText)
import UserInterface.Input (handleEvent)
import UserInterface.Render
  ( drawTCM,
    selectionStyling,
    valueStyling,
  )

-- | Run the app by loading items from the config file, turning them into
--   an initial state and then delegating to Brick. Unless a yaml parsing
--   exception is printed to facilitate config file debugging, there is no
--   failure condition and the program can exit successfully. This situation
--   may change after permission issues are considered. Provide a program
--   description and help text as well.
runApp :: IO ()
runApp = loadConfig >>= synchronizeWithTargetFiles >>= buildInitialState >>= defaultMain tcmApp >>= const exitSuccess

-- | The error message displayed should there be insufficient items specified in the config file.
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
