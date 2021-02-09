-- This file is part of purebred
-- Copyright (C) 2021 Fraser Tweedale
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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Purebred.Plugin.Internal where

import Control.DeepSeq
import Data.Version

import Control.Lens (Lens', lens, set)
import Control.Monad.Reader
import Control.Monad.State

import Data.MIME (MIMEMessage)

import Types (AppState, InternalConfiguration, UserConfiguration)

type Pure = Applicative
type CanIO = MonadIO
type CanRWAppState = MonadState AppState
type CanReadConfig = MonadReader InternalConfiguration

class (CanReadConfig m, CanIO m, CanRWAppState m) => Unconstrained m where
instance (CanReadConfig m, CanIO m, CanRWAppState m) => Unconstrained m where

-- | This is the internal plugin type; a flat record of all hook functions,
-- with relaxed constraints.  Plugin modules should provide a 'Plugin' instead.
-- 'usePlugin' converts a 'Plugin' to a 'PluginDict'.
--
data PluginDict = PluginDict
  { _pluginName :: String
  , _pluginVersion :: Version
  , _pluginBuiltIn :: Bool
  , _configHook :: ConfigHook CanIO
  , _preSendHook :: PreSendHook Unconstrained
  }

instance NFData PluginDict where
  rnf (PluginDict name ver builtin _ _) =
    force name
    `seq` force ver
    `seq` force builtin
    `seq` ()

pluginName :: Lens' PluginDict String
pluginName = lens _pluginName (\s a -> s { _pluginName = a })

pluginVersion :: Lens' PluginDict Version
pluginVersion = lens _pluginVersion (\s a -> s { _pluginVersion = a })

pluginBuiltIn :: Lens' PluginDict Bool
pluginBuiltIn = lens _pluginBuiltIn (\s a -> s { _pluginBuiltIn = a })

configHook :: Lens' PluginDict (ConfigHook CanIO)
configHook = lens _configHook (\s a -> s { _configHook = a })

preSendHook :: Lens' PluginDict (PreSendHook Unconstrained)
preSendHook = lens _preSendHook (\s a -> s { _preSendHook = a })


class Hook t where
  setHook :: t -> PluginDict -> PluginDict

instance (Hook h1, Hook h2) => Hook (h1, h2) where
  setHook (h1, h2) = setHook h1 . setHook h2

instance Hook () where
  setHook _ = id


-- | Process a message before sending.
-- Available capabilities: __all__.
newtype PreSendHook cap =
  PreSendHook { getPreSendHook :: forall m. (cap m) => MIMEMessage -> m MIMEMessage }

instance (forall m. Unconstrained m => cap m) => Hook (PreSendHook cap) where
  setHook (PreSendHook f) = set preSendHook (PreSendHook f)

-- | Process the program configuration at program initialisation.
-- Available capabilities: __CanIO__.
newtype ConfigHook cap =
  ConfigHook
    { getConfigHook
        :: forall m. (cap m)
        => UserConfiguration -> m UserConfiguration
    }

instance (forall m. CanIO m => cap m) => Hook (ConfigHook cap) where
  setHook (ConfigHook f) = set configHook (ConfigHook f)


-- | Plugin constructor.  Apply to the plugin name, version, and hooks.
-- @hooks@ can be a single hook, or a nested tuple of hooks.
--
data Plugin hooks = Plugin String Version hooks

-- | Prepare a plugin for use in the main program.
usePlugin :: (Hook hooks) => Plugin hooks -> PluginDict
usePlugin (Plugin name ver hook) = setHook hook $ PluginDict name ver False
  (ConfigHook pure)
  (PreSendHook pure)
