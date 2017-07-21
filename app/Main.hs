module Main where

import UI.App (theApp, initialState)
import Storage.Notmuch (getMessages)

import Data.Monoid ((<>))
import qualified Brick.Main as M
import Control.Monad (void)
import Options.Applicative hiding (str)

data AppConfig = AppConfig { databaseFilepath :: String }

appconfig :: Parser AppConfig
appconfig = AppConfig <$> strOption ( long "database" <> metavar "DATABASE" <> help "Filepath to notmuch database" )

main :: IO ()
main = do
    msgs <- getMessages . databaseFilepath =<< execParser opts
    void $ M.defaultMain theApp (initialState msgs)
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")
