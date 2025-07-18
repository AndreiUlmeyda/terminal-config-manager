# Common Haskell Patterns in Terminal Config Manager

## Newtype Wrappers

The project uses newtype wrappers for type safety:

```haskell
-- Domain types
newtype AppState = MkAppState ItemsCursor deriving stock (Show, Eq)
newtype TargetValue = MkTargetValue Text deriving stock (Show, Eq, Generic)
newtype Pattern = MkPattern Text deriving stock (Show, Eq, Generic)

-- UI types  
data WidgetId = MkWidgetId deriving stock (Show, Eq, Ord)

-- Custom monads
newtype FsReadIO a = MkFsReadIO {runFsReadIO :: IO a} deriving stock (Functor)
```

## Module Structure Pattern

```haskell
module Domain.SomeModule
  ( SomeType (..),          -- Export constructor and fields
    SomeFunction,           -- Export function only
    someOtherFunction,      -- Export function only
  )
where

-- Explicit imports
import Control.Monad (filterM)
import Data.Text (Text, pack, unpack)

-- Type definitions
data SomeType = MkSomeType
  { field1 :: Text,
    field2 :: Int
  } deriving stock (Show, Eq)

-- Function implementations
someFunction :: Text -> Int -> SomeType
someFunction = MkSomeType
```

## Error Handling Pattern

```haskell
-- In Infrastructure.Errors
errorMsgFailedConfigParsing :: Text
errorMsgFailedConfigParsing = "Failed to parse config file: "

-- Usage in functions
handleParsingErrors :: Either ParseException Config -> IO Config
handleParsingErrors (Right config) = return config
handleParsingErrors (Left parseException) = 
  (die . unpack . append errorMsgFailedConfigParsing . pack . show) parseException
```

## Aeson/YAML Parsing Pattern

```haskell
-- JSON key constants
topLevelConfigElement :: Key
topLevelConfigElement = "config_lines_to_manage"

-- FromJSON instances
instance FromJSON Config where
  parseJSON (Object v) = MkConfig <$> v .: topLevelConfigElement
  parseJSON _ = fail (unpack errorMsgInvalidConfigTopLevel)

instance FromJSON ConfigItem where
  parseJSON (Object v) =
    MkConfigItem
      <$> v .: "title"
      <*> v .: "path" 
      <*> v .: "pattern"
      <*> v .: "targetValue"
      <*> v .: "possibleValues"
```

## Brick UI Pattern

```haskell
-- Event handling
handleEvent :: BrickEvent WidgetId e -> NextAppState
handleEvent event
  | VtyEvent vtye <- event = handleVtyEvent vtye
  | otherwise = continue

-- Widget rendering
drawItem :: Highlighting -> ConfigItem -> Widget WidgetId
drawItem highlighting (MkConfigItem title _ _ (MkTargetValue currentValue) _) =
  hBox
    [ txt title,
      txt titleValueSeparator,
      highlightIf highlighting $ txt currentValue
    ]
```

## Monad Instance Pattern

```haskell
instance Applicative FsReadIO where
  pure = MkFsReadIO . return
  (<*>) = liftA2 ($)

instance Monad FsReadIO where
  (MkFsReadIO a) >>= f = MkFsReadIO (a >>= runFsReadIO . f)
```

## File I/O Pattern

```haskell
-- Restricted file operations
fsIoReadFile :: FilePath -> FsReadIO Text
fsIoReadFile = MkFsReadIO . fmap pack . Strict.readFile

fsIoWriteFile :: FilePath -> Text -> FsReadIO ()
fsIoWriteFile path content = MkFsReadIO $ writeFile path (unpack content)
``` 