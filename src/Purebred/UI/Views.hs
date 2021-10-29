-- This file is part of purebred
-- Copyright (C) 2018-2021 Róman Joost
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
module Purebred.UI.Views
  ( indexView
  , mailView
  , composeView
  , helpView
  , filebrowserView
  , swapWidget
  , visibleViewWidgets
  , focusedViewWidget
  , focusedViewName
  , focusedView
  , toggleLastVisibleWidget
  , resetView
  ) where

import Data.Vector (fromList)
import Data.Maybe (fromMaybe)
import Prelude hiding (splitAt)

import Control.Lens
import Brick.Focus (focusGetCurrent)
import Purebred.Types

import Data.Foldable (toList)


visibleViewWidgets :: AppState -> [[Name]]
visibleViewWidgets s =
  let defaultV = view (asConfig . confDefaultView) s
      focused = fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s
      allLayers = view (asViews . vsViews . ix focused . vLayers) s
   in toList $ toListOf
        (layeriso . traversed . filtered (\t -> view veState t == Visible) . veName)
        <$> allLayers

focusedViewWidget :: AppState -> Name
focusedViewWidget s = view vFocus (focusedView s)

focusedView :: AppState -> View
focusedView s = let focused = view (asViews . vsViews . at (focusedViewName s)) s
                in fromMaybe indexView focused

-- | Swap the widget with the given name with the current widget shown at the bottom of the UI
toggleLastVisibleWidget :: Name -> AppState -> AppState
toggleLastVisibleWidget n s =
  let fallback = focusedViewWidget s
   in s &
      over
        (asViews .
         vsViews . at (focusedViewName s) . _Just . vLayers . ix 0)
        (swapWidget fallback n) .
      set (asViews . vsViews . at (focusedViewName s) . _Just . vFocus) n

swapWidget :: Name -> Name -> Layer -> Layer
swapWidget fallback n m =
  let lastWidget = fromMaybe fallback $ lastOf (layeriso . traverse . filtered (\x -> view veState x == Visible) . veName) m
   in m & set (ix lastWidget . veState) Hidden . set (ix n . veState) Visible

focusedViewName :: AppState -> ViewName
focusedViewName s =
    let defaultV = view (asConfig . confDefaultView) s
    in fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s

resetView :: ViewName -> View -> AppState -> AppState
resetView n = set (asViews . vsViews . ix n)

indexView :: View
indexView =
  View
    { _vLayers =
        fromList
          [ Layer $
            fromList
              [ Tile Visible ListOfThreads
              , Tile Visible StatusBar
              , Tile Visible SearchThreadsEditor
              , Tile Hidden ManageThreadTagsEditor
              , Tile Hidden ComposeFrom
              , Tile Hidden ComposeTo
              , Tile Hidden ComposeSubject
              ]
          ]
    , _vFocus = ListOfThreads
    }

mailView :: View
mailView =
  View
    { _vLayers =
        fromList
          [ Layer $
            fromList
              [ Tile Visible ListOfMails
              , Tile Visible StatusBar
              , Tile Visible ScrollingMailView
              , Tile Hidden ManageMailTagsEditor
              , Tile Hidden MailListOfAttachments
              , Tile Hidden MailAttachmentOpenWithEditor
              , Tile Hidden MailAttachmentPipeToEditor
              , Tile Hidden ScrollingMailViewFindWordEditor
              , Tile Hidden SaveToDiskPathEditor
              , Tile Hidden ComposeTo
              ]
          ]
    , _vFocus = ListOfMails
    }

composeView :: View
composeView =
  View
    { _vLayers =
        fromList
          [ Layer $ fromList [Tile Hidden ConfirmDialog]
          , Layer $
            fromList
              [ Tile Visible ComposeHeaders
              , Tile Visible ComposeListOfAttachments
              , Tile Visible StatusBar
              , Tile Hidden ComposeFrom
              , Tile Hidden ComposeTo
              , Tile Hidden ComposeCc
              , Tile Hidden ComposeBcc
              , Tile Hidden ComposeSubject
              ]
          ]
    , _vFocus = ComposeFrom
    }

helpView :: View
helpView =
  View
    { _vLayers = fromList [Layer $ fromList [Tile Visible ScrollingHelpView]]
    , _vFocus = ScrollingHelpView
    }

filebrowserView :: View
filebrowserView =
  View
    { _vLayers =
        fromList
          [ Layer $
            fromList
              [ Tile Visible ListOfFiles
              , Tile Visible StatusBar
              , Tile Visible ManageFileBrowserSearchPath
              ]
          ]
    , _vFocus = ListOfFiles
    }
