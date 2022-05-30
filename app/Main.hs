module Main where

import Brick

ui :: Widget ()
ui = str "a"

main :: IO ()
main = simpleMain ui