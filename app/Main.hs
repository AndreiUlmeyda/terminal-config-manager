module Main where

import App
  ( buildInitialState,
    tcmApp,
  )
import Brick (defaultMain)
import Config (loadConfig)
import System.Exit (exitSuccess)

main :: IO ()
main = loadConfig >>= buildInitialState >>= defaultMain tcmApp >>= const exitSuccess