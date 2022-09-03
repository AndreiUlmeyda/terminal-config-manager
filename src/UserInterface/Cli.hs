module UserInterface.Cli
  ( provideHelpText,
  )
where

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
  if not (null args)
    then (die . unpack) helpText
    else pure ()

helpText :: Text
helpText =
  "terminal config manager\n  Manage selected values scattered over many \
  \different files quickly\n\nUsage\n  Invoke without any arguments/parameters\n  \
  \Arrow ↑/↓ in order to switch between items\n  Arrow ←/→ to the value\n  \
  \Hit q to quit"
