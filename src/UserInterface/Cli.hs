-- |
-- Module      : Cli
-- Description : Provide help texts in case the program is invoked incorrectly.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module UserInterface.Cli
  ( provideHelpText,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Text
  ( Text,
    unpack,
  )
import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import Prelude

provideHelpText :: IO ()
provideHelpText = do
  args <- getArgs
  unless (null args) ((die . unpack) helpText)

helpText :: Text
helpText =
  "terminal config manager\n  Manage selected values scattered over many \
  \different files quickly\n\nUsage\n  Invoke without any arguments/parameters\n  \
  \Arrow ↑/↓ in order to navigate items\n  Arrow ←/→ to change the value\n  \
  \Hit q to quit"
