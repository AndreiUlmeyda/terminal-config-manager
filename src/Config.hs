module Config (Config (MkConfig), ConfigItem (MkConfigItem), loadConfig) where

import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Yaml
  ( FromJSON (parseJSON),
    Value (Object),
    decodeThrow,
    (.:),
  )
import Prelude hiding (readFile)

testYamlFilePath :: FilePath
testYamlFilePath = "test/data/config.yaml"

data Config = MkConfig [ConfigItem] deriving stock (Eq, Show)

data ConfigItem = MkConfigItem
  { title :: Text,
    path :: FilePath,
    value :: Text,
    possibleValues :: [Text]
  }
  deriving stock (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    MkConfig
      <$> v .: "config_lines_to_manage"
  parseJSON _ = fail "Expected Object for Config value"

instance FromJSON ConfigItem where
  parseJSON (Object v) =
    MkConfigItem
      <$> v .: "title"
      <*> v .: "path"
      <*> v .: "value"
      <*> v .: "possibleValues"
  parseJSON _ = fail "Expected Object for Config value"

loadConfig :: IO Config
loadConfig = readFile testYamlFilePath >>= decodeThrow