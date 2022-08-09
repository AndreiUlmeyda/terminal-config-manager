module FileModification
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

data Content = MkContent Text

modifyFile :: FilePath -> (Content -> Content) -> IO ()
modifyFile path fn = do
  oldContent <- (Strict.readFile) path
  (MkContent newContent) <- pure $ fn $ (MkContent) (pack oldContent)
  (writeFile path) (unpack newContent)