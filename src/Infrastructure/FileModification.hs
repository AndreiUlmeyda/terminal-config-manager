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
  )
import Infrastructure.FsReadIO
  ( FsReadIO,
    fsIoReadFile,
    fsIoWriteFile,
  )

-- | A type representing file content.
newtype Content = MkContent Text deriving stock (Show, Eq)

-- | Given a filepath and a function which takes old content to
--   new content, will apply the function to the file content and,
--   consequently, modify the file on disk.
modifyFile :: FilePath -> (Content -> Content) -> FsReadIO ()
modifyFile path fn = do
  oldContent <- fsIoReadFile path
  (MkContent newContent) <- (pure . fn . MkContent) oldContent
  fsIoWriteFile path newContent
