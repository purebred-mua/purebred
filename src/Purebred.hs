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
import Data.Semigroup ((<>))
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

If recompilation is needed and the library files are in a cabal
newstyle build, use @cabal new-exec purebred@.

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
  module UI.Actions,
  module UI.Index.Keybindings,
  module UI.Mail.Keybindings,
  Event(..),
  Key(..),
  Modifier(..),
  List(..),
  Next,
  getDatabasePath,
  defaultConfig,
  solarizedDark,
  solarizedLight,
  (</>),
  module Control.Lens,
  genBoundary,
  Mailbox(..),
  AddrSpec(..),
  Domain(..),
  purebred) where

import UI.App (theApp, initialState)

import qualified Config.Dyre as Dyre
import qualified Control.DeepSeq
import Control.Monad ((>=>), void)
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as Builder
import Data.Semigroup ((<>))
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import System.Random (RandomGen, getStdGen, randomRs)
import Data.Version (showVersion)
import Paths_purebred (version)

import UI.Index.Keybindings
import UI.Mail.Keybindings
import UI.Actions
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig, solarizedDark, solarizedLight)
import Types

-- re-exports for configuration
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Brick.Main (defaultMain)
import Brick.Types (Next)
import Brick.Widgets.List (List(..))
import Control.Lens ((&), over, set)
import Data.MIME (Mailbox(..), AddrSpec(..), Domain(..))

newtype AppConfig = AppConfig
    { databaseFilepath :: Maybe String
    }

appconfig :: Parser AppConfig
appconfig =
    AppConfig <$> optional
     ( Builder.option
         Builder.str
         (long "database" <> metavar "DATABASE" <>
          help "Filepath to notmuch database") )
     <* Builder.infoOption versionString
         (long "version" <> short 'v' <>
          help "Prints the Purebred version and exits")

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
  let pre = maybe id (set (confNotmuch . nmDatabase) . pure) (databaseFilepath opts)

  -- Set the boundary generator (an INFINITE [Char]) /after/ deepseq'ing :)
  -- FIXME: seems like something that shouldn't be exposed in user config
  b <- genBoundary <$> getStdGen
  let post = set (confComposeView . cvBoundary) b

  cfg' <- post <$> processConfig (pre cfg)

  s <- initialState cfg'
  void $ defaultMain (theApp s) s


-- | Process the user config into an internal configuration, then
-- fully evaluates it.
processConfig :: UserConfiguration -> IO InternalConfiguration
processConfig = fmap Control.DeepSeq.force . (
  (confNotmuch . nmDatabase) id
  >=> confEditor id
  >=> (confFileBrowserView . fbHomePath) id
  )


-- RFC2046 5.1.1
boundaryChars :: String
boundaryChars = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "'()+_,-./:=?"

genBoundary :: RandomGen g => g -> String
genBoundary = filter isBoundaryChar . randomRs (minimum boundaryChars, maximum boundaryChars)
  where
    isBoundaryChar = (`elem` boundaryChars)


purebred :: UserConfiguration -> IO ()
purebred cfg = do
  configDir <- lookupEnv "PUREBRED_CONFIG_DIR"
  ghcOptsEnv <- lookupEnv "GHCOPTS"
  let
    ghcOpts = "-threaded" : maybe [] words ghcOptsEnv
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
