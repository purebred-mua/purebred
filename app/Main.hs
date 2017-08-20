{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.App (theApp, initialState)

import qualified Brick.Main as M
import Control.Monad (void)
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig)
import Data.Maybe (fromMaybe)
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as Builder
import Data.Semigroup ((<>))

data AppConfig = AppConfig { databaseFilepath :: Maybe String }

appconfig :: Parser AppConfig
appconfig =
    AppConfig <$>
    (optional $
     Builder.option
         Builder.str
         (long "database" <> metavar "DATABASE" <>
          help "Filepath to notmuch database"))

main :: IO ()
main = do
    cfg <- execParser opts
    dbpath <- getDatabasePath
    s <- initialState =<< defaultConfig =<< pure (fromMaybe dbpath (databaseFilepath cfg))
    void $ M.defaultMain (theApp s) s
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")
