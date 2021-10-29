-- This file is part of purebred
-- Copyright (C) 2017-2020 Fraser Tweedale and Róman Joost
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

To customise Purebred configuration (including enabling plugins),
create @~\/.config\/purebred\/purebred.hs@.  Apply 'purebred' to the
list of plugins.  Apply 'usePlugin' to each plugin, to prepare it
for use.

The 'tweakConfig' plugin can be used to customise the configuration
to your liking.  For example, the following configuration adds some
custom keybindings:

@
-- ~\/.config\/purebred\/purebred.hs

import Purebred
-- other plugin imports

main :: IO ()
main = 'purebred'
  [ 'usePlugin' $ 'tweakConfig' tweak
  -- , other plugins
  ]

tweak =
    over ('confMailView' . 'mvKeybindings') (mailViewKeybindings <>)
  . over ('confHelpView' . 'hvKeybindings') (scrollKeybindings <>)

scrollKeybindings :: ('Scrollable' w) => ['Keybinding' v w]
scrollKeybindings =
  [ 'Keybinding' (EvKey (KChar 'j') []) ('scrollDown' \`chain\` 'continue')
  , Keybinding (EvKey (KChar 'k') []) ('scrollUp' \`chain\` 'continue')
  , Keybinding (EvKey (KChar 'd') []) ('scrollPageDown' \`chain\` 'continue')
  , Keybinding (EvKey (KChar 'u') []) ('scrollPageUp' \`chain\` 'continue')
  ]

mailViewKeybindings =
  [ Keybinding (EvKey (KChar 'J') []) ('listDown' \`focus\` 'displayMail' \`chain\` continue)
  , Keybinding (EvKey (KChar 'K') []) ('listUp' \`focus\` displayMail \`chain\` continue)
  , Keybinding (EvKey (KChar 'G') []) ('listJumpToEnd' \`chain\` continue)
  , Keybinding (EvKey (KChar 'g') []) ('listJumpToStart' \`chain\` continue)
  ]
  <> scrollKeybindings
@

Then execute @purebred@.  Purebred will detect changes to the custom
configuration, recompile the custom binary, and execute it.

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
  purebred,
  module Purebred.Plugin,
  module Purebred.Plugin.TweakConfig,
  module Purebred.Types,
  module Purebred.Types.Error,
  module Purebred.UI.Actions,
  module Purebred.UI.Index.Keybindings,
  module Purebred.UI.Mail.Keybindings,
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
  solarizedDark,
  mailTagAttr,
  listStateSelectedAttr,
  listStateToggledAttr,
  listStateNewmailAttr,
  (</>),
  module Control.Lens,
  Mailbox(..),
  AddrSpec(..),
  Domain(..),
  sendmail) where

import Purebred.UI.App (theApp, initialState)

import Control.Concurrent (rtsSupportsBoundThreads)
import Purebred.System.Logging (setupLogsink)
import qualified Config.Dyre as Dyre
import qualified Control.DeepSeq
import Control.Monad ((>=>), void, unless)
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as Builder
import qualified Data.Text.Lazy as T
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import Data.Version (showVersion)
import Paths_purebred (version, getLibDir)

import Purebred.UI.Index.Keybindings
import Purebred.UI.Mail.Keybindings
import Purebred.UI.Actions
import Purebred.UI.Status.Main (rescheduleMailcheck)
import Purebred.Config
    (defaultConfig, solarizedDark, mailTagAttr, sendmail,
    listStateSelectedAttr, listStateToggledAttr, listStateNewmailAttr)
import Purebred.Types
import Purebred.Plugin
import Purebred.Plugin.Internal
  ( configHook, pluginBuiltIn, pluginName, pluginVersion )
import Purebred.Plugin.TweakConfig
import Purebred.Storage.Notmuch (getDatabasePath)
import Purebred.Types.Error

-- re-exports for configuration
import qualified Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Brick.BChan (newBChan)
import Brick.Main (customMain)
import Brick.Types (Next)
import Brick.Util (on, fg, bg)
import Brick.AttrMap (AttrName, applyAttrMappings)
import Control.Lens ((&), _head, over, preview, set, view, views)
import Data.Text.Lens (packed)
import Data.MIME (Mailbox(..), AddrSpec(..), Domain(..))

data AppConfig = AppConfig
    { databaseFilepath :: Maybe String
    , searchOverride :: Maybe String
    , debugFile :: Maybe FilePath
    }

appconfig :: String -> Parser AppConfig
appconfig verInfo = AppConfig
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
  <* Builder.infoOption verInfo
    ( long "version"
    <> short 'v'
    <> help "Print the Purebred version and exit"
    )

versionString :: String
versionString = showVersion version

-- | Print full version info about plugins
fullVersionInfo :: [PluginDict] -> String
fullVersionInfo plugins = unlines $
  [ "purebred " <> versionString
  , ""
  , "Configured plugins: "
  , ""
  ] <> (showPlugin <$> filter (views pluginBuiltIn not) plugins)
  where
    showPlugin plug =
      let
        nam = view pluginName plug
        ver = view pluginVersion plug
      in
        "- " <> nam <> " " <> showVersion ver


optParser :: String -> ParserInfo AppConfig
optParser verInfo = info
  (appconfig verInfo <**> helper)
  (fullDesc
   <> progDesc "purebred"
   <> header ("a search based, terminal mail user agent - " <> versionString))

launch :: [String] -> UserConfiguration -> IO ()
launch ghcOpts inCfg = do
  -- Run config hooks before parsing command line options,
  -- so that CLI options can override config.
  let
    plugins = view confPlugins inCfg
    hooks = getConfigHook . view configHook <$> plugins
  cfg <- foldr (>=>) pure hooks inCfg

  -- set the user-specified database path *before* processing config,
  -- to avoid possible error in `notmuch config-get`
  opts <- execParser (optParser (fullVersionInfo plugins))
  let
    pre =
      maybe id (set (confNotmuch . nmDatabase) . pure) (databaseFilepath opts)
      . maybe id (set (confNotmuch . nmSearch)) (view packed <$> searchOverride opts)

  -- Create a channel for sending custom events into Brick event loop.
  -- It gets set in the InternalConfiguration.
  --
  -- There are max 32 elems in chan.  If full, writing will block.  I have
  -- no idea if 32 is a good number or not.
  --
  bchan <- newBChan 32

  -- Create log sink.
  logSink <- setupLogsink (debugFile opts)
  logSink (T.pack "Compile flags: " <> T.intercalate (T.pack " ") (T.pack <$> ghcOpts))
  logSink (T.pack "Opened log file")
  cfg' <- processConfig (InternalConfigurationFields bchan logSink) (pre cfg)

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

-- | Main program entry point.  Apply to a list of plugins (use
-- 'usePlugin' to prepare each plugin for use).
--
purebred :: [PluginDict] -> IO ()
purebred plugins = do
  unless rtsSupportsBoundThreads (error "purebred was not compiled with -threaded")
  configDir <- lookupEnv "PUREBRED_CONFIG_DIR"
  ghcOptsEnv <- maybe [] words <$> lookupEnv "GHCOPTS"
  libdir <- getLibDir

  let
    cfg = over confPlugins (plugins <>) defaultConfig
    ghcOpts = ghcOptsEnv
    dyreParams = (Dyre.newParams "purebred" (launch ghcOpts) (const error))
      { Dyre.configDir = pure <$> configDir
      -- if config dir specified, also use it as cache dir to avoid
      -- clobbering cached binaries for other configurations
      , Dyre.cacheDir = pure <$> configDir
      , Dyre.includeDirs = [libdir]
      , Dyre.ghcOpts = ghcOpts
      }
  Dyre.wrapMain dyreParams cfg
