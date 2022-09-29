-- This file is part of purebred
-- Copyright (C) 2018-2021 Róman Joost and Fraser Tweedale
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

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Purebred.UI.Status.Main where

import Brick.BChan (BChan, writeBChan)
import Brick.Types (Widget)
import Brick.Widgets.Core
  (Padding(..), hBox, txt, str, withAttr, (<+>), strWrap,
  emptyWidget, padRight, padLeft, padLeftRight)
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.List  as L
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.FileBrowser as FB
import Control.Monad.Except (runExceptT)
import Control.Monad (void)
import Control.Lens
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (Text)
import Data.Text.Zipper (cursorPosition)

import Purebred.UI.Draw.Main (fillLine)
import Purebred.UI.Utils (titleize)
import Purebred.UI.Views (focusedViewWidget, focusedViewName)
import Purebred.Types
import Purebred.Config
  ( statusbarAttr, statusbarErrorAttr, statusbarInfoAttr, statusbarWarningAttr )
import Purebred.Storage.Client (Server, countMessages)
import Purebred.UI.Widgets (editEditorL)

checkForNewMail :: BChan PurebredEvent -> Server -> Text -> Delay -> IO ()
checkForNewMail chan server query delay = do
  r <- runExceptT (countMessages query server)
  case r of
    Left _ -> pure ()
    Right n -> notify n *> rescheduleMailcheck chan server query delay
  where
    notify = writeBChan chan . NotifyNewMailArrived

rescheduleMailcheck :: BChan PurebredEvent -> Server -> Text -> Delay -> IO ()
rescheduleMailcheck chan server query delay =
  void $ forkIO (threadDelay (toMilisecond delay) *> checkForNewMail chan server query delay)
    where
      toMilisecond (Seconds x) = x * 1000000
      toMilisecond (Minutes x) = x * 60 * 1000000

data StatusbarContext a
    = ListContext a
    | EditorContext a
    | ErrorContext a
    deriving (Show)

renderUserMessage :: UserMessage -> Widget Name
renderUserMessage (UserMessage _ (Warning t)) = withAttr statusbarWarningAttr $ hCenter $ txt t
renderUserMessage (UserMessage _ (Info t)) = withAttr statusbarInfoAttr $ hCenter $ txt t
renderUserMessage (UserMessage _ (Error e)) = withAttr statusbarErrorAttr $ hCenter $ strWrap (show e)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asUserMessage s of
        Just m -> renderUserMessage m
        Nothing ->
            case focusedViewWidget s of
                SearchThreadsEditor -> renderStatusbar (view (asThreadsView . miSearchThreadsEditor . editEditorL) s) s
                ManageMailTagsEditor -> renderStatusbar (view (asThreadsView . miMailTagsEditor) s) s
                ManageThreadTagsEditor -> renderStatusbar (view (asThreadsView . miThreadTagsEditor) s) s
                MailAttachmentOpenWithEditor -> renderStatusbar (view (asMailView . mvOpenCommand) s) s
                MailAttachmentPipeToEditor -> renderStatusbar (view (asMailView . mvPipeCommand) s) s
                ScrollingMailViewFindWordEditor -> renderStatusbar (view (asMailView . mvFindWordEditor) s) s
                SaveToDiskPathEditor -> renderStatusbar (view (asMailView . mvSaveToDiskPath) s) s
                ListOfThreads -> renderStatusbar (view (asThreadsView . miThreads) s) s
                ListOfMails -> renderStatusbar (view (asThreadsView . miMails) s) s
                ScrollingMailView -> renderStatusbar (view (asThreadsView . miMails) s) s
                ComposeListOfAttachments -> renderStatusbar (views (asCompose . cAttachments) lwl s) s
                MailListOfAttachments -> renderStatusbar (views (asMailView . mvAttachments) lwl s) s
                ListOfFiles -> renderStatusbar (view (asFileBrowser . fbEntries) s) s
                ComposeTo -> renderStatusbar (view (asCompose . cTo . editEditorL) s) s
                ComposeFrom -> renderStatusbar (view (asCompose . cFrom . editEditorL) s) s
                ComposeSubject -> renderStatusbar (view (asCompose . cSubject . editEditorL) s) s
                _ -> withAttr statusbarAttr $ str "Purebred: " <+> fillLine

class WithContext a where
  renderContext :: AppState -> a -> Widget Name

instance WithContext (ListWithLength t e) where
  renderContext _ = currentItemW

instance WithContext (E.Editor Text Name) where
  renderContext _ = str . show . cursorPosition . view E.editContentsL

instance WithContext (FB.FileBrowser Name) where
  renderContext _ _ = emptyWidget

renderStatusbar :: WithContext w => w -> AppState -> Widget Name
renderStatusbar w s = withAttr statusbarAttr $ hBox
  [ str "Purebred: "
  , renderContext s w
  , padLeftRight 1 (str "[")
  , renderNewMailIndicator s
  , renderMatches s
  , padLeft (Pad 1) (str "]")
  , fillLine
  , txt (
      titleize (focusedViewName s) <> "-"
      <> titleize (focusedViewWidget s) <> " "
      )
  ]

renderMatches :: AppState -> Widget n
renderMatches s =
  case lengthOf (asMailView . mvBody . mbMatches . traverse) s of
    0 -> emptyWidget
    n -> let
            i = view (asMailView . mvSearchIndex) s
          in
            str (show (i + 1) <> " of " <> show n <> " matches")

renderNewMailIndicator :: AppState -> Widget n
renderNewMailIndicator s =
  let newMailCount = view (asThreadsView . miNewMail) s
      indicator = str $ "New: " <> show newMailCount
   in padRight (Pad 1) indicator

currentItemW :: ListWithLength t e -> Widget n
currentItemW (ListWithLength l len) = str $
  maybe
    "No items"
    (\i -> "Item " <> show (i + 1) <> " of " <> maybe "?" show len)
    (view L.listSelectedL l)

-- | Convenience function for promoting a brick list to a 'ListWithLength',
-- using 'length' on the underlying list.
lwl :: (Foldable t) => L.GenericList Name t e -> ListWithLength t e
lwl l = ListWithLength l (views L.listElementsL (Just . length) l)
