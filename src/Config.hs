module Config (Config (MkConfig), ConfigItem (..), loadConfig, TargetValue (MkTargetValue), Pattern (MkPattern)) where

import Data.Text (Text)
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import GHC.Generics (Generic)
import System.Exit (die)

testYamlFilePath :: FilePath
testYamlFilePath = "test/data/config.yaml"

data Config = MkConfig [ConfigItem] deriving stock (Eq, Show)

data ConfigItem = MkConfigItem
  { title :: Text,
    path :: FilePath,
    matchingPattern :: Pattern,
    targetValue :: TargetValue,
    possibleValues :: [TargetValue]
  }
  deriving stock (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    MkConfig
      <$> v .: "config_lines_to_manage"
  parseJSON _ = fail "The top level of the config file should be an object named 'config_lines_to_manage'"

instance FromJSON ConfigItem where
  parseJSON (Object v) =
    MkConfigItem
      <$> v .: "title"
      <*> v .: "path"
      <*> v .: "pattern"
      <*> v .: "targetValue"
      <*> v .: "possibleValues"
  parseJSON _ = fail "Each config entry is expected to contain 4 items. 'title', 'path', 'targetValue' and 'possibleValues'"

data TargetValue = MkTargetValue Text deriving stock (Show, Eq, Generic)

instance FromJSON TargetValue

data Pattern = MkPattern Text deriving stock (Show, Eq, Generic)

instance FromJSON Pattern

-- | Parse the YAML config file into the types specified above. Throw an error if something is missing.
loadConfig :: IO Config
loadConfig = decodeFileEither testYamlFilePath >>= handleParsingErrors

-- | Indicate that the yaml errors encountered here happened during config file parsing.
handleParsingErrors :: Either ParseException Config -> IO Config
handleParsingErrors (Right config) = return config
handleParsingErrors (Left parseException) = die $ "There was an error while parsing the configuration file. The details are:\n" ++ (show parseException)
