-- |
-- Module      : Errors
-- Description : Define error messages for other modules.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Infrastructure.Errors
  ( errorMsgInvalidConfigTopLevel,
    errorMsgInvalidConfigElements,
    errorMsgFailedConfigParsing,
    errorMsgNoConfigEntries,
    errorMsgUnableToDetermineCurrentValue,
    generateCurrentValueErrorMessage,
  )
where

import Data.Text
  ( Text,
    unpack,
  )

type ErrorMessage = String

errorMsgInvalidConfigTopLevel :: ErrorMessage
errorMsgInvalidConfigTopLevel = "The top level of the config file should be an object named 'config_lines_to_manage'"

errorMsgInvalidConfigElements :: ErrorMessage
errorMsgInvalidConfigElements = "Each config entry is expected to contain 4 items. 'title', 'path', 'targetValue' and 'possibleValues'"

-- | The message displayed if errors occured during parsing of the yaml config file.
errorMsgFailedConfigParsing :: ErrorMessage
errorMsgFailedConfigParsing = "An error occured while parsing the configuration file. The details are:\n"

-- | The error message displayed should there be an insufficient number of items specified in the config file.
errorMsgNoConfigEntries :: ErrorMessage
errorMsgNoConfigEntries = "There are no entries in the config file."

errorMsgUnableToDetermineCurrentValue :: ErrorMessage
errorMsgUnableToDetermineCurrentValue = "An error occured while trying to determine the current value inside of "

type MatchingPattern = Text

generateCurrentValueErrorMessage :: FilePath -> MatchingPattern -> ErrorMessage
generateCurrentValueErrorMessage path pat =
  errorMsgUnableToDetermineCurrentValue
    ++ path
    ++ " using the pattern "
    ++ unpack pat