module Main where

import App
  ( buildInitialState,
    tcmApp,
  )
import Brick (defaultMain)
import Config (loadConfig)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  config <- loadConfig
  initialState <- buildInitialState config
  endState <- defaultMain tcmApp initialState
  exitSuccess