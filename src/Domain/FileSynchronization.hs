module Domain.FileSynchronization
  ( synchronizeWithTargetFiles,
    extractValue,
    removeBeforeAndIncluding,
    removeAfterAndIncluding,
  )
where

import Data.Text
  ( Text,
    breakOn,
    drop,
    length,
    null,
    pack,
    splitOn,
  )
import Domain.ValueSelection
  ( valueMarker,
  )
import Infrastructure.Config
  ( Config (..),
    ConfigItem (..),
    Pattern (..),
    TargetValue (..),
    unwrapPattern,
  )
import Infrastructure.Errors
  ( generateCurrentValueErrorMessage,
  )
import Infrastructure.FileModification
  ( Content (..),
  )
import System.Exit
  ( die,
  )
import System.IO.Strict as Strict
  ( readFile,
  )
import Prelude hiding
  ( drop,
    length,
    null,
  )

-- | For a given item there is no guarantee that the target value as it is
--   configured is identical to the corresponding value in the target file at
--   the current moment. Therefore, on startup, we need to make sure that the
--   displayed value is in sync with the target file. This is achieved by
--   extracting by reading each file and extracting the target value based on
--   the pattern.
--
--   This is done in sequence. Substituting 'sequence' for 'parallel' may appear
--   as an obvious optimization, it may lead to issues with open file desciptors
--   when there are multiple items pointing to the same file. If the
--   optimization is needed it could work if the items are grouped by path
--   first, but even then they could be pointing to the same file through
--   symbolic links.
synchronizeWithTargetFiles :: Config -> IO Config
synchronizeWithTargetFiles (MkConfig items) = do
  newItems <- (sequence . map currentValueFromFile) items
  (return . MkConfig) newItems

currentValueFromFile :: ConfigItem -> IO ConfigItem
currentValueFromFile item = do
  currentFileContent <- (fmap pack . Strict.readFile . path) item
  currentValue <- valueFromContent item currentFileContent
  return $ item {targetValue = currentValue}

valueFromContent :: ConfigItem -> Text -> IO TargetValue
valueFromContent item currentFileContent
  | Just v <- extractValue (matchingPattern item) valueMarker (MkContent currentFileContent) = pure v
  | otherwise = die $ generateCurrentValueErrorMessage (path item) (unwrapPattern (matchingPattern item))

type ValueMarker = Text

extractValue :: Pattern -> ValueMarker -> Content -> Maybe TargetValue
extractValue (MkPattern pat) marker (MkContent content)
  | any null [pat, marker, content] = Nothing
  | otherwise =
      ( Just
          . MkTargetValue
          . removeAfterAndIncluding newLine
          . removeAfterAndIncluding afterMarker
          . removeBeforeAndIncluding beforeMarker
      )
        content
  where
    beforeMarker = (splitOn marker pat) !! 0
    afterMarker = (splitOn marker pat) !! 1

newLine :: Text
newLine = "\n"

removeBeforeAndIncluding :: Text -> Text -> Text
removeBeforeAndIncluding "" = id
removeBeforeAndIncluding needle = drop (length needle) . snd . breakOn needle

removeAfterAndIncluding :: Text -> Text -> Text
removeAfterAndIncluding "" = id
removeAfterAndIncluding needle = fst . breakOn needle
