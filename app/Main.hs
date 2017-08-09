module Main where

import UI.App (theApp, initialState)

import qualified Brick.Main as M
import Control.Monad (void)
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig)

main :: IO ()
main = do
  s <- initialState =<< defaultConfig =<< getDatabasePath
  void $ M.defaultMain (theApp s) s
