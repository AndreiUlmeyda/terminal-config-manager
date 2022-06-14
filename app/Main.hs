module Main where

import Brick (defaultMain)
import Lib
  ( buildInitialState,
    tcmApp,
  )
import System.Exit (exitSuccess)

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  exitSuccess