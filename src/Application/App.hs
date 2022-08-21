-- |
-- Module      : App
-- Description : Build and run a Brick app after loading the config file.
--   Additionally, provide a program description and help text when using the
--   flag --help.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Application.App
  ( buildInitialState,
    tcmApp,
    AppState,
    runApp,
  )
where

import Brick
  ( App (..),
    AttrName,
    attrMap,
    attrName,
    defaultMain,
    showFirstCursor,
  )
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
  )
import Data.List.NonEmpty as NE
  ( nonEmpty,
  )
import Data.Text
  ( unpack,
  )
import Domain.FileSynchronization
  ( synchronizeWithTargetFiles,
  )
import Domain.State
  ( AppState (MkAppState),
    ResourceName,
  )
import Graphics.Vty.Attributes
  ( Attr,
    currentAttr,
  )
import Infrastructure.Config
  ( Config (MkConfig),
    loadConfig,
  )
import Infrastructure.Errors
  ( errorMsgNoConfigEntries,
  )
import System.Exit
  ( die,
    exitSuccess,
  )
import UserInterface.Cli
  ( provideHelpText,
  )
import UserInterface.Input
  ( handleEvent,
  )
import UserInterface.Render
  ( attributeNameSelected,
    attributeNameValue,
    drawTCM,
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
runApp =
  loadConfig
    >>= synchronizeWithTargetFiles
    >>= buildInitialState
    >>= defaultMain tcmApp
    >>= const exitSuccess

-- | For this application only event handling, drawing and some attributes need
--   to be implemented. The rest are default implementations.
tcmApp :: App AppState e ResourceName
tcmApp =
  App
    { appHandleEvent = handleEvent,
      appDraw = drawTCM,
      appAttrMap = (const . attrMap currentAttr) attributeMap,
      appChooseCursor = showFirstCursor,
      appStartEvent = pure
    }

attributeMap :: [(AttrName, Attr)]
attributeMap =
  [ ((attrName . unpack) attributeNameSelected, selectionStyling),
    ((attrName . unpack) attributeNameValue, valueStyling)
  ]

-- | Wrap the config file entries in a nonempty list.
buildInitialState :: Config -> IO AppState
buildInitialState (MkConfig configItems) =
  case NE.nonEmpty configItems of
    Nothing -> die errorMsgNoConfigEntries
    Just ne -> (pure . MkAppState . makeNonEmptyCursor) ne
