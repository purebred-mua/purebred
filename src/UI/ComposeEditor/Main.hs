{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main (attachmentsEditor) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (padRight, txt, (<+>),)
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Part(..))
import Control.Lens (view)
import Data.Maybe (fromMaybe)

import UI.Utils (focusedViewWidget)
import Types

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let hasFocus = ListOfAttachments == focusedViewWidget s ComposeFrom
        attachmentsList = L.renderList (\_ i -> renderPart i) hasFocus (view (asCompose . cAttachments) s)
    in attachmentsList

renderPart :: Part -> Widget Name
renderPart p = let pType = partType p
                   pFilename = fromMaybe "--" (partFilename p)
               in padRight Max (txt pFilename) <+> txt pType
