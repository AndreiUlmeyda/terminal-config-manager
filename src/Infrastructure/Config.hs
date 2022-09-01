-- |
-- Module      : Config
-- Description : Expose a function which parses the contents of the config file
--  and returns a list of items specified in that file.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Infrastructure.Config
  ( Config (MkConfig),
    ConfigItem (..),
    loadConfig,
    TargetValue (MkTargetValue),
    Pattern (MkPattern),
    unwrapPattern,
  )
where

import Data.Aeson
  ( Key,
  )
import Data.Text
  ( Text,
    append,
    pack,
    unpack,
  )
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import GHC.Generics (Generic)
import Infrastructure.Errors
  ( errorMsgFailedConfigParsing,
    errorMsgInvalidConfigElements,
    errorMsgInvalidConfigTopLevel,
  )
import System.Exit (die)

-- | The file path of the configuration file used during development.
testYamlFilePath :: FilePath
testYamlFilePath = "test/data/config.yaml"

-- | A configurations consists of multiple items.
data Config = MkConfig [ConfigItem] deriving stock (Eq, Show)

-- | A configuration item contains a description and information needed to
--   identify the correct string substitution inside of the target file.
data ConfigItem = MkConfigItem
  { title :: Text,
    path :: FilePath,
    matchingPattern :: Pattern,
    targetValue :: TargetValue,
    possibleValues :: [TargetValue]
  }
  deriving stock (Eq, Show)

topLevelConfigElement :: Key
topLevelConfigElement = "config_lines_to_manage"

configElementTitle :: Key
configElementTitle = "title"

configElementPath :: Key
configElementPath = "path"

configElementPattern :: Key
configElementPattern = "pattern"

configElementValue :: Key
configElementValue = "targetValue"

configElementPossibleValues :: Key
configElementPossibleValues = "possibleValues"

instance FromJSON Config where
  parseJSON (Object v) =
    MkConfig
      <$> v .: topLevelConfigElement
  parseJSON _ = fail (unpack errorMsgInvalidConfigTopLevel)

instance FromJSON ConfigItem where
  parseJSON (Object v) =
    MkConfigItem
      <$> v .: configElementTitle
      <*> v .: configElementPath
      <*> v .: configElementPattern
      <*> v .: configElementValue
      <*> v .: configElementPossibleValues
  parseJSON _ = fail (unpack errorMsgInvalidConfigElements)

-- | A substring inside of the file you want to manage. Its occurence inside of
--   that file will be substituted when changing the corresponding item.
data TargetValue = MkTargetValue Text deriving stock (Show, Eq, Generic)

-- | In order to avoid a cyclic dependency during error message generation, a
--   way to get the value out of a pattern needs to be provided. Providing one
--   by making Pattern a record type created issues with parsing so will need
--   to suffice for the moment.
unwrapPattern :: Pattern -> Text
unwrapPattern (MkPattern pat) = pat

instance FromJSON TargetValue

-- | In order to identify which part of a target file to modify, a pattern
--   needs to be specified. This pattern must contain the string "{{value}}"
--   and surrounding text. The amount of surrounding text needs to be carefully
--   considered. It should be long enough so that only the one intended line
--   of the target file will match it.
data Pattern = MkPattern Text deriving stock (Show, Eq, Generic)

instance FromJSON Pattern

-- | Parse the YAML config file into the types specified above. Throw an error
--   if something is missing.
loadConfig :: () -> IO Config
loadConfig = const $ decodeFileEither testYamlFilePath >>= handleParsingErrors

-- | Indicate that the yaml errors encountered here happened during config file
--   parsing.
handleParsingErrors :: Either ParseException Config -> IO Config
handleParsingErrors (Right config) = return config
handleParsingErrors (Left parseException) = (die . unpack . append errorMsgFailedConfigParsing . pack . show) parseException
