{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Status.Main where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, withAttr, (<+>), strWrap, emptyWidget)
import Brick.Focus (focusGetCurrent)
import qualified Brick.Widgets.List  as L
import qualified Brick.Widgets.Edit  as E
import Control.Lens.Getter (view)
import Data.MIME (MIMEMessage)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Zipper (cursorPosition)
import Data.Maybe (fromMaybe)
import UI.Draw.Main (fillLine)
import Types
import Config.Main (statusbarAttr, statusbarErrorAttr)
import Error (Error)

data StatusbarContext a
    = ListContext a
    | EditorContext a
    | ErrorContext a
    deriving (Show)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> renderStatusbar e
        Nothing -> case fromMaybe ListOfThreads $ focusGetCurrent (view (asViews . vsFocus) s) of
          SearchThreadsEditor -> renderStatusbar (view (asMailIndex . miSearchThreadsEditor) s)
          ManageMailTagsEditor -> renderStatusbar (view (asMailIndex . miMailTagsEditor) s)
          ListOfThreads -> renderStatusbar (view (asMailIndex . miListOfThreads) s)
          ListOfMails -> renderStatusbar (view (asMailIndex . miListOfMails) s)
          ScrollingMailView -> renderStatusbar (view (asMailView . mvMail) s)
          ComposeTo -> renderStatusbar (view (asCompose . cTo) s)
          ComposeFrom -> renderStatusbar (view (asCompose . cFrom) s)
          ComposeSubject -> renderStatusbar (view (asCompose . cSubject) s)

class WithContext a where
  renderContext :: a -> Widget Name

instance WithContext (L.List Name NotmuchThread) where
  renderContext = currentItemW

instance WithContext (L.List Name NotmuchMail) where
  renderContext = currentItemW

instance WithContext (E.Editor Text Name) where
  renderContext e = str $ show $ cursorPosition $ view E.editContentsL e

instance WithContext (Maybe MIMEMessage) where
  renderContext _ = emptyWidget

instance WithContext Error where
  renderContext e = withAttr statusbarErrorAttr $ strWrap (show e)

renderStatusbar :: WithContext w => w -> Widget Name
renderStatusbar w =
  withAttr statusbarAttr $
    str "Purebred: " <+> renderContext w <+> fillLine

currentItemW :: Show e => L.List Name e -> Widget Name
currentItemW l = str $
  maybe
    "No items"
    (\i -> "Item " <> show (i + 1) <> " of " <> total)
    (view L.listSelectedL l)
  where
      total = show $ length $ view L.listElementsL l
