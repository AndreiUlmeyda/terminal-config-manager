-- |
-- Module      : FsReadIO
-- Description : Provide a custom IO Monad which is restricted to file
--   access. This monad only allows specific file system operations
--   and prevents arbitrary IO actions.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Infrastructure.FsReadIO
  ( FsReadIO, -- Export type but NOT constructor
    fsIoReadFile,
    fsIoWriteFile,
    runFsReadIO, -- Controlled runner
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

-- | A restricted IO monad that only allows file system operations.
-- The constructor is not exported, preventing arbitrary IO actions.
newtype FsReadIO a = MkFsReadIO (IO a)
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

-- | Read a file and return its contents as Text.
-- This is the only way to perform file reading in this monad.
fsIoReadFile :: FilePath -> FsReadIO Text
fsIoReadFile path = MkFsReadIO (pack <$> Strict.readFile path)

-- | Write Text content to a file.
-- This is the only way to perform file writing in this monad.
fsIoWriteFile :: FilePath -> Text -> FsReadIO ()
fsIoWriteFile path content = MkFsReadIO (writeFile path (unpack content))

-- | Execute a restricted file system computation.
-- This is the only way to run FsReadIO actions.
runFsReadIO :: FsReadIO a -> IO a
runFsReadIO (MkFsReadIO action) = action
