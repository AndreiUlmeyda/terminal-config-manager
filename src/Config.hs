module Config (Config (MkConfig), ConfigItem (MkConfigItem), loadConfig) where

import Data.ByteString as BS (readFile)
import Data.Text (Text)
import Data.Yaml as Y
  ( FromJSON (parseJSON),
    Value (Object),
    decodeThrow,
    (.:),
  )

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
  parseJSON (Y.Object v) =
    MkConfig
      <$> v .: "config_lines_to_manage"
  parseJSON _ = fail "Expected Object for Config value"

instance FromJSON ConfigItem where
  parseJSON (Y.Object v) =
    MkConfigItem
      <$> v .: "title"
      <*> v .: "path"
      <*> v .: "value"
      <*> v .: "possibleValues"
  parseJSON _ = fail "Expected Object for Config value"

loadConfig :: IO Config
loadConfig = do
  configYaml <- BS.readFile testYamlFilePath
  config <- Y.decodeThrow configYaml
  return config