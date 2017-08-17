{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.App (theApp, initialState)

import qualified Brick.Main as M
import Control.Monad (void)
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig)
import Options.Applicative hiding (str)
import Data.Semigroup ((<>))

data AppConfig = AppConfig { databaseFilepath :: String }

appconfig :: Parser AppConfig
appconfig = AppConfig <$> strOption ( long "database" <> metavar "DATABASE" <> help "Filepath to notmuch database" )

main :: IO ()
main = do
    cfg <- execParser opts
    s <- initialState =<< defaultConfig =<< pure (databaseFilepath cfg)
    void $ M.defaultMain (theApp s) s
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")
