module Main where

import UI.App (theApp, initialState)

import qualified Brick.Main as M
import Control.Monad (void)
import Storage.Notmuch (getDatabasePath)

main :: IO ()
main = do
    databaseFilepath <- getDatabasePath
    s <- initialState databaseFilepath
    void $ M.defaultMain theApp s
