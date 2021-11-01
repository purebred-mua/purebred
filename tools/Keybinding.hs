-- This file is part of purebred
-- Copyright (C) 2020 Róman Joost
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

{-

Generates asciidoc compatible table markup listing key bindings for
each widget.

-}

{-# LANGUAGE OverloadedStrings #-}

import Brick.BChan (newBChan)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Control.DeepSeq
import Purebred.Config (defaultConfig)
import Control.Monad ((>=>))
import Purebred.UI.Help.Main (createKeybindingIndex, KeybindingHelp(..), HelpIndex)
import Purebred.UI.App (initialState)
import Control.Lens (set, view, ifoldr, traversed, toListOf, ix)
import Purebred.Types


main :: IO ()
main = do 
  bchan <- newBChan 32
  cfg' <- processConfig (InternalConfigurationFields bchan dummyLogSink) defaultConfig
  s <- initialState cfg'
  T.putStrLn (renderHelpAsAsciidoc s cfg')

dummyLogSink :: LT.Text -> IO ()
dummyLogSink _ = pure ()

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

-- | Render all defined key bindings as an asciidoc compatible table
--
renderHelpAsAsciidoc :: AppState -> InternalConfiguration -> T.Text
renderHelpAsAsciidoc s cfg =
  let index = createKeybindingIndex cfg
  in ifoldr (renderKeybinding index) mempty (view (asViews . vsViews) s)

-- Cheap way to weed out any widgets not receiving keyboard events and
-- not having event handlers registered.
receivesInput :: Name -> Bool
receivesInput StatusBar = False
receivesInput ListOfMails = False
receivesInput ComposeHeaders = False
receivesInput _ = True

renderKeybinding :: HelpIndex -> ViewName -> View -> T.Text -> T.Text
renderKeybinding index vn v sibling =
  mconcat
    [ sibling
    , "\n=== "
    , T.pack $ show vn
    , foldr
        (\name rendered ->
           if receivesInput name
             then renderKbGroupAsText index name rendered
             else rendered)
        mempty $
      toListOf (vLayers . traversed . layeriso . traversed . veName) v
    ]

renderKbGroupAsText :: HelpIndex -> Name -> T.Text -> T.Text
renderKbGroupAsText index name sibling =
  let kbs = toListOf (ix name . traversed) index
  in mconcat
  [ sibling
  , "\n"
  , "\n==== "
  , T.pack (show name)
  , "\n|===\n|Shortcut |Purpose |Raw Keycode\n\n"
  , T.intercalate "\n" (renderKeybindingText <$> kbs)
  , "\n|===" ]

renderKeybindingText :: KeybindingHelp -> T.Text
renderKeybindingText (KeybindingHelp keys actions raw) =
   T.intercalate
        "\n"
        [ "|kbd:[" <> keys <> "]"
        , "|" <> actions
        , "|`" <> T.pack raw <> "`"
        ]
