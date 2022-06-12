module Main where

import Brick (defaultMain)
import Lib
  ( buildInitialState,
    tcmApp,
  )

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tcmApp initialState
  print endState