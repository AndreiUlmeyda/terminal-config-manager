-- |
-- Module      : FileSynchronization
-- Description : Expose a function which, for a given config item will read the
--  corresponding file and determine the current value as it would be identified
--  by the pattern.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Domain.FileSynchronization
  ( synchronizeWithTargetFiles,
    extractValue,
  )
where

import Data.Text as T
  ( Text,
    breakOn,
    drop,
    isInfixOf,
    length,
    lines,
    null,
    pack,
    splitOn,
    unpack,
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
  ( errorInvalidPattern,
    generateCurrentValueErrorMessage,
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
import Prelude as P

-- | For a given item there is no guarantee that the target value as it is
--   configured is identical to the corresponding value in the target file at
--   the current moment. Therefore, on startup, we need to make sure that the
--   displayed value is in sync with the target file. This is achieved by
--   extracting the value by reading each file and determining the target value
--   based on the pattern.
--
--   This is done in sequence. Trying this in parallel may appear
--   as an obvious optimization but it may lead to issues with open file
--   desciptors when there are multiple items pointing to the same file. If the
--   optimization is needed it could work if the items are grouped by path
--   first, but even then they could be pointing to the same file through
--   symbolic links.
synchronizeWithTargetFiles :: Config -> IO Config
synchronizeWithTargetFiles (MkConfig items) = fmap MkConfig (mapM currentValueFromFile items)

-- | Read a file corresponding to an item and extract the target value according
--   to the pattern.
currentValueFromFile :: ConfigItem -> IO ConfigItem
currentValueFromFile item = do
  currentFileContent <- (fmap pack . Strict.readFile . path) item
  currentValue <- valueFromContent item currentFileContent
  return $ item {targetValue = currentValue}

-- | Extract the target value of an item using its pattern.
valueFromContent :: ConfigItem -> Text -> IO TargetValue
valueFromContent item currentFileContent
  | Just v <- extractValue (matchingPattern item) valueMarker (MkContent currentFileContent) = pure v
  | otherwise = die $ unpack $ generateCurrentValueErrorMessage (path item) (unwrapPattern (matchingPattern item))

type ValueMarker = Text

-- | Extract a value from a given Text according to a pattern containing a value
--   marker.
extractValue :: Pattern -> ValueMarker -> Content -> Maybe TargetValue
extractValue (MkPattern pat) marker (MkContent content)
  | any T.null [pat, marker, content] = Nothing
  | pat == marker = (Just . MkTargetValue . head . T.lines) content
  | P.length linesContainingMarker /= 1 = (error . unpack) errorInvalidPattern
  | otherwise = Just $ MkTargetValue $ extractValueBetweenMarkers beforeMarker afterMarker (head linesContainingMarker)
  where
    splitAtValueMarker = splitOn marker pat
    beforeMarker = head splitAtValueMarker
    afterMarker = splitAtValueMarker !! 1
    linesContainingMarker = filter (\a -> T.isInfixOf beforeMarker a && T.isInfixOf afterMarker a) (T.lines content)

    -- Extract text between two markers using standard Text operations
    extractValueBetweenMarkers :: Text -> Text -> Text -> Text
    extractValueBetweenMarkers before after line
      | T.null before && T.null after = line
      | T.null before = fst $ T.breakOn after line
      | T.null after = T.drop (T.length before) $ snd $ T.breakOn before line
      | otherwise =
          let afterPrefix = T.drop (T.length before) $ snd $ T.breakOn before line
           in fst $ T.breakOn after afterPrefix
