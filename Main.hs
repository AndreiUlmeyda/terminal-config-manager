-- |
-- Module      : Main
-- Description : The entry point of the program.
-- Copyright   : (c) Adrian Schurz, 2022
-- License     : MIT
-- Maintainer  : adrian.schurz@check24.com
-- Stability   : experimental
module Main where

import Application.App (runApp)

-- | Keep the mandatory main module thin such that all of the code is organized
--   in proper directories.
main :: IO ()
main = runApp