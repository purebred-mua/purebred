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

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Plugins for adjusting Purebred's configuration.

You can use 'tweakConfig' to apply a pure transformation to the
'UserConfiguration' at startup.

@
-- override notmuch database and "new" tag
tweak =
    set (confNotmuch . nmDatabase) (pure "\/home\/alice\/mail")
  . set (confNotmuch . nmNewTag) "inbox"

main = purebredWithPlugins
  [ usePlugin $ 'tweakConfig' tweak
  , ...
  ]
@

The 'tweakConfigWithIO' variant allows the transform function to
perform I/O.

-}
module Purebred.Plugin.TweakConfig where

import Purebred.Plugin
import Types (UserConfiguration)

tweakConfig
  :: (UserConfiguration -> UserConfiguration)
  -> Plugin (ConfigHook Pure)
tweakConfig hook =
  Plugin "Purebred.Plugin.TweakConfig" (ConfigHook (pure . hook))

tweakConfigWithIO
  :: (forall m. CanIO m => UserConfiguration -> m UserConfiguration)
  -> Plugin (ConfigHook CanIO)
tweakConfigWithIO hook =
  Plugin "Purebred.Plugin.TweakConfig (IO)" (ConfigHook hook)
