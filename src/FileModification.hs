module FileModification
  ( modifyFile,
    Content (..),
  )
where

import Brick
  ( EventM,
  )
import Control.Monad.IO.Class
  ( MonadIO (liftIO),
  )
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import System.IO.Strict as Strict
  ( readFile,
  )

data Content = MkContent Text

modifyFile :: FilePath -> (Content -> Content) -> EventM n ()
modifyFile path fn = do
  oldContent <- (liftIO . Strict.readFile) path
  (MkContent newContent) <- fmap fn ((liftIO . pure . MkContent) (pack oldContent))
  (liftIO . writeFile path) (unpack newContent)