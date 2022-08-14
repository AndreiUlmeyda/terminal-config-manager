-- |
-- Module      : FileModification
-- Description : Expose a type representing target file content and
--  a function to modify the content of a file.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Infrastructure.FileModification
  ( modifyFile,
    Content (..),
  )
where

import Data.Text
  ( Text,
    pack,
    unpack,
  )
import System.IO.Strict as Strict
  ( readFile,
  )

-- | A type representing file content.
data Content = MkContent Text

-- | Given a filepath and a function which takes old content to
--   new content, will apply the function to the file content and,
--   consequently modify the file on disk.
modifyFile :: FilePath -> (Content -> Content) -> IO ()
modifyFile path fn = do
  oldContent <- (Strict.readFile) path
  (MkContent newContent) <- pure $ fn $ (MkContent) (pack oldContent)
  (writeFile path) (unpack newContent)