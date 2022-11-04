-- |
-- Module      : FsReadIO
-- Description : Provide a custom IO Monad which is restricted to file
--   access.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Infrastructure.FsReadIO
  ( fsIoReadFile,
    fsIoWriteFile,
    FsReadIO (MkFsReadIO, runFsReadIO),
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import System.IO.Strict as Strict
  ( readFile,
  )

newtype FsReadIO a = MkFsReadIO {runFsReadIO :: IO a} deriving stock (Functor)

fsIoReadFile :: FilePath -> FsReadIO Text
fsIoReadFile = MkFsReadIO . fmap pack . Strict.readFile

fsIoWriteFile :: FilePath -> Text -> FsReadIO ()
fsIoWriteFile path content = MkFsReadIO $ writeFile path (unpack content)

instance Applicative FsReadIO where
  pure = MkFsReadIO . return
  (<*>) = liftA2 ($)

instance Monad FsReadIO where
  (MkFsReadIO a) >>= f = MkFsReadIO (a >>= runFsReadIO . f)