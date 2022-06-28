module Config (Config (Config), ConfigItem (ConfigItem), loadConfig) where

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

data Config = Config [ConfigItem] deriving stock (Eq, Show)

data ConfigItem = ConfigItem
  { title :: Text,
    path :: FilePath,
    value :: Text,
    possibleValues :: [Text]
  }
  deriving stock (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config
      <$> v .: "config_lines_to_manage"
  parseJSON _ = fail "Expected Object for Config value"

instance FromJSON ConfigItem where
  parseJSON (Y.Object v) =
    ConfigItem
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