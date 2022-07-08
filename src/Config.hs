module Config (Config (MkConfig), ConfigItem (..), loadConfig, TargetValue (MkTargetValue), Pattern (MkPattern)) where

import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Yaml
  ( FromJSON (parseJSON),
    Value (Object),
    decodeThrow,
    (.:),
  )
import GHC.Generics (Generic)
import Prelude hiding (readFile)

testYamlFilePath :: FilePath
testYamlFilePath = "test/data/config.yaml"

data Config = MkConfig [ConfigItem] deriving stock (Eq, Show)

data ConfigItem = MkConfigItem
  { title :: Text,
    path :: FilePath,
    pattern :: Pattern,
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
  parseJSON _ = fail "Each config entry is expected to contain 4 items. 'title', 'path', 'value and 'possibleValues'"

data TargetValue = MkTargetValue Text deriving stock (Show, Eq, Generic)

instance FromJSON TargetValue

data Pattern = MkPattern Text deriving stock (Show, Eq, Generic)

instance FromJSON Pattern

loadConfig :: IO Config
loadConfig = readFile testYamlFilePath >>= decodeThrow