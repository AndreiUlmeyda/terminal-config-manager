module Main where

import App
  ( buildInitialState,
    tcmApp,
  )
import Brick (defaultMain)
import Config (loadConfig)
import System.Exit (exitSuccess)

-- | This is an application to manage selected values scattered over many different files quickly.
--
--   A common use case will probably be to quickly switch single values inside of a set of config files
--   without having to:

--     navigate to the file
--     open it in a text editor
--     search the target value
--     modify it
--     save the file
--
--  In order to know which files to manipulate a few things will need to be specified beforehand inside of a config
--  file for this application and for each value we want to manage:
--
--     a title
--     the absolute path of the target (config) file
--     the current value of the config entry in question
--     a pattern with which to identify the parts of the target file surrounding the config entry in question
--     a list of possible values for the config entry in question
--
--  The application displays one line per managed value, its title and the current value.
--
--  The up/down arrow keys select the value to change, left/right arrows change the value to the next or previous
--  possible value.

main :: IO ()
main = loadConfig >>= buildInitialState >>= defaultMain tcmApp >>= const exitSuccess