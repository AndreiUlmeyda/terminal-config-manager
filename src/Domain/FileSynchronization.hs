module Domain.FileSynchronization
  ( synchronizeWithTargetFiles,
    extractValue,
  )
where

import Data.Text
  ( Text,
    breakOn,
    null,
    pack,
    splitOn,
    stripPrefix,
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
  ( null,
  )

synchronizeWithTargetFiles :: Config -> IO Config
synchronizeWithTargetFiles config = do
  currentValues <- currentValuesFromFile config
  return currentValues

-- | Substituting 'sequence' for 'parallel' may lead to issues with open file
--   desciptors when there are multiple items pointing to the same file. If the
--   optimization is needed it could work if the items are grouped by path
--   first, but even then they could be pointing to the same file through
--   symbolic links.
currentValuesFromFile :: Config -> IO Config
currentValuesFromFile (MkConfig items) = do
  newItems <- (sequence . map currentValueFromFile) items
  (return . MkConfig) newItems

currentValueFromFile :: ConfigItem -> IO ConfigItem
currentValueFromFile item = do
  currentFileContent <- Strict.readFile (path item)
  currentValue <- case extractValue (matchingPattern item) valueMarker (MkContent (pack currentFileContent)) of
    (Just v) -> pure v
    Nothing -> die $ generateCurrentValueErrorMessage (path item) (unwrapPattern (matchingPattern item))
  return $ item {targetValue = currentValue}

type ValueMarker = Text

-- from pattern and valueMarker and content to value
-- 'BOOKANCY_ENV={{value}}' and '{{value}}' and '...tert werden\nBOOKANCY_ENV=local\n# Host (...' to 'local'
-- 1 split pattern at valueMarker -> ('BOOKANCY_ENV=','')
-- 2 drop until and including the first result of step 1 from the content
-- 3 prune the end at newline
-- 4 prune again at the second result of step 1

extractValue :: Pattern -> ValueMarker -> Content -> Maybe TargetValue
extractValue (MkPattern pat) marker (MkContent content)
  | any null [pat, marker, content] = Nothing
  | otherwise = (Just . MkTargetValue) targetVal
  where
    targetVal = case stripPrefix beforeMarker (textAfter beforeMarker content) of
      Nothing -> (textAfter beforeMarker content)
      Just withoutPrefix -> textBeforeUntilNewLine afterMarker withoutPrefix
    beforeMarker = (splitOn marker pat) !! 0
    afterMarker = (splitOn marker pat) !! 1

newLine :: Text
newLine = "\n"

textBeforeUntilNewLine :: Text -> Text -> Text
textBeforeUntilNewLine needle = untilNewLine . textBefore needle

untilNewLine :: Text -> Text
untilNewLine = textBefore newLine

textBefore :: Text -> Text -> Text
textBefore needle = fst . breakOn' needle

textAfter :: Text -> Text -> Text
textAfter needle = snd . breakOn' needle

-- | A version of breakOn which tolerates an empty 'needle' and, in such a case,
--   just returns a tuple of containings two times the 'haystack'.
breakOn' :: Text -> Text -> (Text, Text)
breakOn' needle haystack
  | null needle = (haystack, haystack)
  | otherwise = breakOn needle haystack
