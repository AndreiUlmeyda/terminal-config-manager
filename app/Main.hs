module Main where

import App
  ( buildInitialState,
    tcmApp,
  )
import Brick (defaultMain)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  exitSuccess