{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.App (theApp, initialState)

import qualified Brick.Main as M
import Control.Monad (void)
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig)
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
    dbfp <- case databaseFilepath cfg of
          Nothing -> getDatabasePath
          Just fp -> pure fp
    s <- initialState =<< defaultConfig dbfp
    void $ M.defaultMain (theApp s) s
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")
