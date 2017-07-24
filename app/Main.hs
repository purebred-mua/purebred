module Main where

import UI.App (theApp, initialState)

import Data.Monoid ((<>))
import qualified Brick.Main as M
import Control.Monad (void)
import Options.Applicative hiding (str)

data AppConfig = AppConfig { databaseFilepath :: String }

appconfig :: Parser AppConfig
appconfig = AppConfig <$> strOption ( long "database" <> metavar "DATABASE" <> help "Filepath to notmuch database" )

main :: IO ()
main = do
    cfg <- execParser opts
    s <- initialState (databaseFilepath cfg)
    void $ M.defaultMain theApp s
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")
