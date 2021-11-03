-- This file is part of purebred
-- Copyright (C) 2017-2021 Róman Joost and Fraser Tweedale
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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Types related to the UI.

-}
module Purebred.Types.UI
  (
    Name(..)

  , ViewSettings(..)
  , vsViews
  , vsFocusedView
  , ViewName(..)
  , View(..)
  , ViewState(..)
  , vFocus
  , vLayers
  , Layer(..)
  , Layers
  , layeriso
  , Tile(..)
  , veName
  , veState

  , Toggleable
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified Brick.Focus as Brick

{-# ANN module "HLint: ignore Avoid lambda" #-}

-- | Widget identifiers. Each rendered widget has one unique
-- identifier in Purebred's UI.
--
data Name
  = ComposeBcc
  | ComposeCc
  | ComposeFrom
  | ComposeHeaders
  | ComposeListOfAttachments
  | ComposeSubject
  | ComposeTo
  | ConfirmDialog
  | ListOfFiles
  | ListOfMails
  | ListOfThreads
  | MailAttachmentOpenWithEditor
  | MailAttachmentPipeToEditor
  | MailListOfAttachments
  | ManageFileBrowserSearchPath
  | ManageMailTagsEditor
  | ManageThreadTagsEditor
  | SaveToDiskPathEditor
  | SearchThreadsEditor
  | ScrollingHelpView
  | ScrollingMailView
  | ScrollingMailViewFindWordEditor
  | StatusBar
  deriving (Eq, Ord, Show)

data ViewName
  = Threads
  | Mails
  | ViewMail
  | ComposeView
  | Help
  | FileBrowser
  deriving (Eq, Ord, Show, Generic, NFData)

data ViewState
  = Hidden
  | Visible
  deriving (Eq, Show)

-- | A view element is a name for a widget with a given view state
data Tile = Tile ViewState Name
  deriving (Eq, Show)

veName :: Lens' Tile Name
veName f (Tile a b) = fmap (\b' -> Tile a b') (f b)

veState :: Lens' Tile ViewState
veState f (Tile a b) = fmap (\a' -> Tile a' b) (f a)
{-# ANN veState ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

type Layers = V.Vector Layer

data View = View
  { _vFocus :: Name
  , _vLayers :: Layers
  }

vLayers :: Lens' View Layers
vLayers = lens _vLayers (\settings x -> settings { _vLayers = x })

vFocus :: Lens' View Name
vFocus = lens _vFocus (\settings x -> settings { _vFocus = x})

-- | A layer is a view element with a list of widgets and their view state
newtype Layer = Layer (V.Vector Tile)
  deriving (Eq, Show)

type instance Index Layer = Name
type instance IxValue Layer = Tile

instance Ixed Layer where
  ix = tile

layeriso :: Iso' Layer (V.Vector Tile)
layeriso = iso (\(Layer xs) -> xs) Layer

tile :: Name -> Traversal' Layer Tile
tile k = layeriso . traversed . filtered (\x -> k == view veName x)

data ViewSettings = ViewSettings
    { _vsViews :: Map.Map ViewName View
    , _vsFocusedView :: Brick.FocusRing ViewName
    }

vsViews :: Lens' ViewSettings (Map.Map ViewName View)
vsViews = lens _vsViews (\settings x -> settings { _vsViews = x })

vsFocusedView :: Lens' ViewSettings (Brick.FocusRing ViewName)
vsFocusedView = lens _vsFocusedView (\settings x -> settings { _vsFocusedView = x})

-- | An item carrying it's selected state.
-- Used in order to mark list items.
--
type Toggleable a = (Bool, a)
