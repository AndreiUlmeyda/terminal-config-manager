module UserInterface.Cli
  ( provideHelpText,
  )
where

-- cmd line description and help text

import Data.Text
  ( Text,
    unpack,
  )
import Options.Applicative
  ( InfoMod,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    helper,
    hsubparser,
    info,
    progDesc,
    (<**>),
  )
import Prelude

description :: Text
description =
  "Manage selected values scattered over many different files quickly. Arrow up/\
  \down switches between items. Arrow left/right changes the value. Hit q to quit."

title :: Text
title = "terminal-config-manager"

provideHelpText :: IO ()
provideHelpText = execParser argumentParser

argumentParser :: ParserInfo a
argumentParser = info (hsubparser mempty <**> helper) programDescription

programDescription :: InfoMod a
programDescription =
  fullDesc
    <> (progDesc . unpack) description
    <> (header . unpack) title
