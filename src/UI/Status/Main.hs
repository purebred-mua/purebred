{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Status.Main where

import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core
       (txt, str, withAttr, (<+>), strWrap, padRight)
import qualified Brick.Widgets.List  as L
import qualified Brick.Widgets.Edit  as E
import Control.Lens (view)
import Data.MIME (MIMEMessage)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Zipper (cursorPosition)
import UI.Draw.Main (fillLine)
import UI.Utils (focusedViewWidget, focusedViewName, titleize)
import Types
import Config.Main (statusbarAttr, statusbarErrorAttr)

data StatusbarContext a
    = ListContext a
    | EditorContext a
    | ErrorContext a
    deriving (Show)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> withAttr statusbarErrorAttr $ strWrap (show e)
        Nothing ->
            case focusedViewWidget s ListOfThreads of
                SearchThreadsEditor -> renderStatusbar (view (asMailIndex . miSearchThreadsEditor) s) s
                ManageMailTagsEditor -> renderStatusbar (view (asMailIndex . miMailTagsEditor) s) s
                ManageThreadTagsEditor -> renderStatusbar (view (asMailIndex . miThreadTagsEditor) s) s
                ListOfThreads -> renderStatusbar (view (asMailIndex . miListOfThreads) s) s
                ListOfMails -> renderStatusbar (view (asMailIndex . miListOfMails) s) s
                ScrollingMailView -> renderStatusbar (view (asMailView . mvMail) s) s
                ListOfAttachments -> renderStatusbar (view (asCompose . cAttachments) s) s
                ListOfFiles -> renderStatusbar (view (asFileBrowser . fbEntries) s) s
                ComposeTo -> renderStatusbar (view (asCompose . cTo) s) s
                ComposeFrom -> renderStatusbar (view (asCompose . cFrom) s) s
                ComposeSubject -> renderStatusbar (view (asCompose . cSubject) s) s
                _ -> withAttr statusbarAttr $ str "Purebred: " <+> fillLine

class WithContext a where
  renderContext :: AppState -> a -> Widget Name

instance WithContext (L.List Name NotmuchThread) where
  renderContext _ = currentItemW

instance WithContext (L.List Name NotmuchMail) where
  renderContext _ = currentItemW

instance WithContext (L.List Name (Bool, FileSystemEntry)) where
  renderContext _ = currentItemW

instance WithContext (L.List Name MIMEMessage) where
  renderContext _ = currentItemW

instance WithContext (E.Editor Text Name) where
  renderContext _ = str . show . cursorPosition . view E.editContentsL

instance WithContext (Maybe MIMEMessage) where
  renderContext s _ = currentItemW (view (asMailIndex . miListOfMails) s)

renderStatusbar :: WithContext w => w -> AppState -> Widget Name
renderStatusbar w s =
    withAttr statusbarAttr
    $ str "Purebred: "
    <+> padRight (Pad 1) (renderContext s w)
    <+> fillLine
    <+> padRight (Pad 1) (txt (titleize (focusedViewName s) <> "-" <> titleize (focusedViewWidget s ListOfThreads)))

currentItemW :: Show e => L.List Name e -> Widget Name
currentItemW l = str $
  maybe
    "No items"
    (\i -> "Item " <> show (i + 1) <> " of " <> total)
    (view L.listSelectedL l)
  where
      total = show $ length $ view L.listElementsL l
