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

{-|

The Purebred plugin system.

-}
module Purebred.Plugin
  (
  -- * Using plugins
  -- $using
    usePlugin
  , PluginDict

  -- ** Advanced usage for security-conscious users
  -- $paranoia

  -- * Defining plugins
  -- $defining
  , Plugin(..)

  -- ** Capabilities
  -- $capabilities
  , Pure, CanIO, CanReadConfig, CanRWAppState, Unconstrained

  -- ** Hooks
  -- $hooks
  , PreSendHook(..)
  , ConfigHook(..)
  ) where

import Purebred.Plugin.Internal

{- $using

To use plugins, update your @~\/.config\/purebred\/purebred.hs@ to
use the 'Purebred.purebred' entry point with a list of
plugins.  Apply 'usePlugin' to each plugin value to prepare it for
use.

@
main = 'Purebred.purebred'
  [ 'usePlugin' ('Purebred.Plugin.TweakConfig.tweakConfig' tweak)
  , 'usePlugin' Purebred.Plugin.ICU.plugin
  ]
@

Some plugins could require additional configuration arguments.
Refer to the documentation for individual plugin modules to find out
how to configure them.

Purebred plugins have the type @'Plugin' hooks@.  @hooks@ encodes
the hooks implemented by the plugin, and the required capabilities.
In this way, the plugin type is (machine-enforced) documentation
about the possible range of plugin behaviour.

-}

{- $paranoia

Security-conscious users can use type annotations to protect against
plugins acquiring new capabilities upon upgrade.  In the example
below, the ICU plugin is fixed at @ConfigHook Pure@.  If a new
version changes to @ConfigHook CanIO@ or uses additional hooks, the
program will not compile.  The user can decide whether to accept the
new type (by updating the annotation), or remove the plugin.

@
main = 'Purebred.purebred'
  [ 'usePlugin' (Purebred.Plugin.TweakConfig.tweakConfig tweak)
  , 'usePlugin' (Purebred.Plugin.ICU.plugin :: 'Plugin' ('ConfigHook' 'Pure'))
  ]
@

-}

{- $defining

The type of a plugin is @'Plugin' hooks@ where @hooks@ is either a
single hook function newtype, or a nested tuple of hook functions.
Many plugins will only use a single hook.  Some will use multiple
hooks.

To define a plugin, apply the 'Plugin' constructor to a plugin name,
'Version', and hooks.  If the plugin requires user configuration (as
in the following example), use function argument(s).

@
-- | Add a signature to outgoing messages
signature :: Text -> Plugin (PreSendHook CanReadConfig)
signature str = Plugin "UserAgent" version (PreSendHook hook)
  where
  hook msg = view confCharsets >>= manipulateTheMessage msg
@

Each hook has a set of /available/ capabilities, and plugins declare
the /required/ capabilities for the implemented hooks.  Available
capabilities differ and are documented at each hook newtype, and in
the table below.

-}

{- $capabilities

The plugin system defines the following capabilities.

+---------------------+--------------------------------+
| Capabilitity        | Description                    |
|                     |                                |
+=====================+================================+
| 'Pure'              | The hook function can only     |
|                     | perform a pure computation     |
|                     | based on its input.            |
+---------------------+--------------------------------+
| 'CanReadConfig'     | The hook function can read the |
|                     | configuration via 'ask'.       |
+---------------------+--------------------------------+
| 'CanRWAppState'     | The hook function can read and |
|                     | write the 'AppState' via 'get' |
|                     | and 'put'.                     |
+---------------------+--------------------------------+
| 'CanIO'             | The hook function can perform  |
|                     | I/O via 'liftIO'.              |
+---------------------+--------------------------------+
| 'Unconstrained'     | Meta-capability that implies   |
|                     | all defined capabilities.      |
+---------------------+--------------------------------+

Not all capabilities are available to all hooks (see hooks table
below).


-}

{- $hooks

+---------------------+----------------------------+------------------+
| Hook                | Description                | Available        |
|                     |                            | capabilities     |
+=====================+============================+==================+
| 'ConfigHook'        | Modify config at startup   | 'CanIO'          |
+---------------------+----------------------------+------------------+
| 'PreSendHook'       | Modify or process message  | 'Unconstrained'  |
|                     | before sending             |                  |
+---------------------+----------------------------+------------------+

-}
