module UserInterface.Cli
  ( provideHelpText,
  )
where

-- cmd line description and help text
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

provideHelpText :: IO ()
provideHelpText = execParser argumentParser

argumentParser :: ParserInfo a
argumentParser = info (hsubparser mempty <**> helper) programDescription

programDescription :: InfoMod a
programDescription =
  fullDesc
    <> progDesc "Manage selected values scattered over many different files quickly. Arrow up/down switches between items. Arrow left/right changes the value. Hit q to quit."
    <> header "terminal-config-manager"
