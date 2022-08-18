module Domain.TargetFileSynchronization
  ( synchronizeWithTargetFiles,
    extractValue,
  )
where

import Data.Text
  ( Text,
    null,
    pack,
  )
import Domain.ValueSelection
  ( valueMarker,
  )
import Infrastructure.Config
  ( Config (..),
    ConfigItem (..),
    Pattern (..),
    TargetValue (..),
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

-- | Substituting 'sequence' for 'parallel' may lead to issues with open file desciptors
--   when there are multiple items pointing to the same file. If the optimization
--   is needed it could work if the work items are grouped by path first, but even
--   then, they could point to the same file through symbolic links.
currentValuesFromFile :: Config -> IO Config
currentValuesFromFile (MkConfig items) = do
  newItems <- (sequence . map currentValueFromFile) items
  (return . MkConfig) newItems

currentValueFromFile :: ConfigItem -> IO ConfigItem
currentValueFromFile item = do
  currentFileContent <- Strict.readFile (path item)
  currentValue <- case extractValue (matchingPattern item) valueMarker (MkContent (pack currentFileContent)) of
    (Just v) -> pure v
    Nothing -> die ""
  return $ item {targetValue = currentValue}

type ValueMarker = Text

-- from pattern and valueMarker and content to value
-- 'BOOKANCY_ENV={{value}}' and '{{value}}' and '...tert werden\nBOOKANCY_ENV=local\n# Host (...' to 'local'
-- 1 split pattern at valueMarker -> ('BOOKANCY_ENV=','')
-- 2 drop until and including the first result of step 1 from the content
-- 3 prune the end at newline
-- 4 prune again at the second result of step 1

extractValue :: Pattern -> ValueMarker -> Content -> Maybe TargetValue
extractValue (MkPattern matchingPattern) marker (MkContent content)
  | all (not . null) [matchingPattern, marker, content] = Just $ MkTargetValue content
  | otherwise = Nothing