-- This file is part of purebred
-- Copyright (C) 2017-2018 Fraser Tweedale and Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{- |

= Synopsis

To customise purebred configuration, create
@~\/.config\/purebred\/purebred.hs@ and change the default config to
your liking.  For example, the following configuration adds some
custom keybindings:

@
import Purebred

scrollKeybindings :: ('Scrollable' w) => ['Keybinding' v w]
scrollKeybindings =
  [ 'Keybinding' (EvKey (KChar 'j') []) ('scrollDown' ``chain`` 'continue')
  , Keybinding (EvKey (KChar 'k') []) ('scrollUp' \`chain\` continue)
  , Keybinding (EvKey (KChar 'd') []) ('scrollPageDown' \`chain\` continue)
  , Keybinding (EvKey (KChar 'u') []) ('scrollPageUp' \`chain\` continue)
  ]

mailViewKeybindings =
  [ Keybinding (EvKey (KChar 'J') []) ('listDown' ``chain'`` 'displayMail' \`chain\` continue)
  , Keybinding (EvKey (KChar 'K') []) ('listUp' \`chain'` displayMail \`chain\` continue)
  , Keybinding (EvKey (KChar 'G') []) ('listJumpToEnd' \`chain\` continue)
  , Keybinding (EvKey (KChar 'g') []) ('listJumpToStart' \`chain\` continue)
  ]
  <> scrollKeybindings

main = 'purebred' $ tweak 'defaultConfig' where
  tweak =
    over ('confMailView' . 'mvKeybindings') (mailViewKeybindings <>)
    . over ('confHelpView' . 'hvKeybindings') (scrollKeybindings <>)
@

The invoke the program, just run @purebred@:

= Overriding the config directory

If you want to override the configuration file location, use the
@PUREBRED_CONFIG_DIR@ environment variable.  The configuration file,
located in this directory, must always be name @purebred.hs@.

The binary is normally cached in @~\/.cache\/purebred\/@.  If you
override the configuration directory, the configuration directory is
also used as the cache directory, to avoid clobbering the cached
binary for the other configurations.

= Recompilation

== Passing extra arguments to GHC

If you want to pass extra arguments to GHC, use the @GHCOPTS@
environment variable.  For example, to compile with profiling:

@
GHCOPTS="-prof" purebred
@

Note that the presence of the @GHCOPTS@ environment variable will
not force recompilation to occur.  To force recompilation you could
make a benign change to the custom configuration file (e.g. add or
remove an empty line).

== With Cabal (newstyle)

purebred should be able to determine the correct package-id and pass
this datum to GHC when recompiling.  Whether running from the
repository or not, recompile should /just work/, regardless of where
you are running the program from.

== With Stack

If recompilation is needed and you used @stack@ to build and install the
program, it will not be able to find the libraries:

> ftweedal% purebred
> Configuration '/home/ftweedal/.config/purebred/purebred.hs' changed. Recompiling.
> Error occurred while loading configuration file.
> Launching custom binary /home/ftweedal/.cache/purebred/purebred-linux-x86_64
> 
> purebred-linux-x86_64: 
> /home/ftweedal/.config/purebred/purebred.hs:4:1: error:
>     Could not find module ‘Purebred’
>     Use -v to see a list of the files searched for.
>   |
> 4 | import Purebred
>   | ^^^^^^^^^^^^^^^
> 
> CallStack (from HasCallStack):
>   error, called at src/Purebred.hs:205:32 in purebred-0.1.0.0-8yyFpK6IBghCAYUvNAhJRk:Purebred

To avoid this, don't use stack.  But if you insist, you can run
@stack exec purebred@ from the source tree.

-}
module Purebred (
  module Types,
  module Error,
  module UI.Actions,
  module UI.Index.Keybindings,
  module UI.Mail.Keybindings,
  module Graphics.Vty.Attributes,
  Event(..),
  Key(..),
  Modifier(..),
  Next,
  AttrName,
  on,
  fg,
  bg,
  applyAttrMappings,
  getDatabasePath,
  defaultConfig,
  solarizedDark,
  mailTagAttr,
  (</>),
  module Control.Lens,
  genBoundary,
  Mailbox(..),
  AddrSpec(..),
  Domain(..),
  purebred) where

import UI.App (theApp, initialState)

import qualified Config.Dyre as Dyre
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import qualified Control.DeepSeq
import Control.Monad ((>=>), forever, void)
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as Builder
import Data.List (elemIndex, isInfixOf, isPrefixOf)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, joinPath, splitPath)
import System.FilePath.Posix ((</>))
import System.IO (BufferMode(LineBuffering), IOMode(AppendMode), hSetBuffering, openFile)
import System.Random (RandomGen, getStdGen, randomRs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Paths_purebred (version, getLibDir)

import UI.Index.Keybindings
import UI.Mail.Keybindings
import UI.Actions
import UI.Status.Main (rescheduleMailcheck)
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig, solarizedDark, mailTagAttr)
import Types
import Error

-- re-exports for configuration
import qualified Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Brick.BChan (newBChan)
import Brick.Main (customMain)
import Brick.Types (Next)
import Brick.Util (on, fg, bg)
import Brick.AttrMap (AttrName, applyAttrMappings)
import Control.Lens ((&), _head, over, preview, set, view)
import Data.Text.Lens (packed)
import Data.MIME (Mailbox(..), AddrSpec(..), Domain(..))

data AppConfig = AppConfig
    { databaseFilepath :: Maybe String
    , searchOverride :: Maybe String
    , debugFile :: Maybe FilePath
    }

appconfig :: Parser AppConfig
appconfig = AppConfig
  <$> optional
    ( Builder.option Builder.str
      ( long "database"
      <> metavar "DATABASE"
      <> help "Filepath to notmuch database"
      )
    )
  <*> optional
    ( Builder.option Builder.str
      ( long "search"
      <> metavar "SEARCH-TERM"
      <> help "Override the initial notmuch search"
      )
    )
  <*> optional
    ( Builder.option Builder.str
      ( long "debug"
      <> metavar "FILE"
      <> help "Write debug information to FILE"
      )
    )
  <* Builder.infoOption versionString
    ( long "version"
    <> short 'v'
    <> help "Print the Purebred version and exit"
    )

versionString :: String
versionString = showVersion version

optParser :: ParserInfo AppConfig
optParser = info
  (appconfig <**> helper)
  (fullDesc
   <> progDesc "purebred"
   <> header ("a search based, terminal mail user agent - " <> versionString))

launch :: UserConfiguration -> IO ()
launch cfg = do

  -- set the user-specified database path *before* processing config,
  -- to avoid possible error in `notmuch config-get`
  opts <- execParser optParser
  let
    pre =
      maybe id (set (confNotmuch . nmDatabase) . pure) (databaseFilepath opts)
      . maybe id (set (confNotmuch . nmSearch)) (view packed <$> searchOverride opts)

  -- Set the boundary generator (an INFINITE [Char]) /after/ deepseq'ing :)
  -- FIXME: seems like something that shouldn't be exposed in user config
  b <- genBoundary <$> getStdGen

  -- Create a channel for sending custom events into Brick event loop.
  -- It gets set in the InternalConfiguration.
  --
  -- There are max 32 elems in chan.  If full, writing will block.  I have
  -- no idea if 32 is a good number or not.
  --
  bchan <- newBChan 32

  -- Create log sink.
  logSink <- case debugFile opts of
    Nothing -> pure $ \_ -> pure ()
    Just fp -> do
      h <- openFile fp AppendMode
      hSetBuffering h LineBuffering
      T.hPutStrLn h $ T.pack "Opened log file"
      q <- newTQueueIO
      _ <- forkIO $ forever $ atomically (readTQueue q) >>= T.hPutStrLn h
      pure $ atomically . writeTQueue q

  cfg' <- processConfig (bchan, b, logSink) (pre cfg)

  s <- initialState cfg'
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty

  let query = view (confNotmuch . nmHasNewMailSearch) cfg'
      delay = view (confNotmuch . nmHasNewMailCheckDelay) cfg'
      dbpath = view (confNotmuch . nmDatabase) cfg'
  maybe (pure ()) (rescheduleMailcheck bchan dbpath query) delay

  void $ customMain initialVty buildVty (Just bchan) (theApp s) s


-- | Fully evaluate the 'UserConfiguration', then set the extra data to
-- turn it into an 'InternalConfiguration'.
processConfig
  :: InternalConfigurationFields
  -> UserConfiguration
  -> IO InternalConfiguration
processConfig z = fmap (set confExtra z . Control.DeepSeq.force) . unIO
  where
  unIO =
    (confNotmuch . nmDatabase) id
    >=> confEditor id
    >=> (confFileBrowserView . fbHomePath) id


-- RFC2046 5.1.1
boundaryChars :: String
boundaryChars = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "'()+_,-./:=?"

genBoundary :: RandomGen g => g -> String
genBoundary = filter isBoundaryChar . randomRs (minimum boundaryChars, maximum boundaryChars)
  where
    isBoundaryChar = (`elem` boundaryChars)


-- | Try to determine a package-related args from the libdir
--
guessPackageArgs :: FilePath -> [String]
guessPackageArgs dir =
  let
    path = dropTrailingPathSeparator <$> splitPath dir
    reversedPath = case reverse path of
      ("lib" : xs) -> xs  -- if component is "lib", drop it
      xs -> xs
    isCabalStore = [".cabal", "store"] `isInfixOf` path
    packageId =
      let
        f s
          | isCabalStore && ("purebred-" <> versionString <> "-") `isPrefixOf` s =
              Just s -- cabal newstyle install
          | otherwise = Nothing
      in
        maybe [] (\s -> ["-package-id", s]) (preview _head reversedPath >>= f)
    packageDb
      | isCabalStore =
          maybe []
            (\i -> ["-package-db", joinPath (take (i + 3) path <> ["package.db"])])
            (elemIndex ".cabal" path)
      | otherwise = []
  in
    packageDb <> packageId


purebred :: UserConfiguration -> IO ()
purebred cfg = do
  configDir <- lookupEnv "PUREBRED_CONFIG_DIR"
  ghcOptsEnv <- lookupEnv "GHCOPTS"

  libdir <- getLibDir

  let
    ghcOpts = "-threaded" : maybe [] words ghcOptsEnv <> guessPackageArgs libdir
    dyreParams = Dyre.defaultParams
      { Dyre.projectName = "purebred"
      , Dyre.realMain = launch
      , Dyre.showError = const error
      , Dyre.configDir = pure <$> configDir
      -- if config dir specified, also use it as cache dir to avoid
      -- clobbering cached binaries for other configurations
      , Dyre.cacheDir = pure <$> configDir
      , Dyre.ghcOpts = ghcOpts
      }
  Dyre.wrapMain dyreParams cfg
