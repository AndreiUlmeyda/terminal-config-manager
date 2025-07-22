-- |
-- Module      : Cli
-- Description : Provide help texts in case the program is invoked incorrectly.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module UserInterface.Cli
  ( parseCliArgs,
    CliOptions (..),
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

-- | Command line options
newtype CliOptions = CliOptions
  { configFile :: Maybe FilePath
  }
  deriving stock (Show, Eq)

-- | Parse command line arguments and return options or show help
parseCliArgs :: IO CliOptions
parseCliArgs = do
  args <- getArgs
  case args of
    [] -> return $ CliOptions Nothing
    ["--help"] -> (die . unpack) helpText
    ["-h"] -> (die . unpack) helpText
    ["--config", path] -> return $ CliOptions (Just path)
    _ -> (die . unpack) $ "Invalid arguments. " <> helpText

helpText :: Text
helpText =
  "terminal config manager\n  Manage selected values scattered over many \
  \different files quickly\n\nUsage\n  terminal-config-manager [OPTIONS]\n\n\
  \Options:\n  --config PATH    Use specific config file\n  --help, -h       Show this help\n\n\
  \Navigation:\n  Arrow ↑/↓/w/s in order to navigate items\n  Arrow ←/→/a/d to change the value\n  \
  \Hit q to quit"
