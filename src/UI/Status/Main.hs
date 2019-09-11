-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Status.Main where

import Brick.BChan (BChan, writeBChan)
import Brick.Types (Widget)
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Core
  (hBox, txt, str, withAttr, (<+>), strWrap, padLeftRight,
  emptyWidget)
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.List  as L
import qualified Brick.Widgets.Edit  as E
import Control.Monad.Except (runExceptT)
import Control.Monad (void)
import Control.Lens (view, views, to, has, _Just, preview, non)
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (Text)
import Data.Text.Zipper (cursorPosition)

import UI.Draw.Main (fillLine)
import UI.Utils (titleize)
import UI.Views (focusedViewWidget, focusedViewName)
import Types
import Error
import Config.Main (statusbarAttr, statusbarErrorAttr)
import qualified Storage.Notmuch as Notmuch

checkForNewMail :: BChan PurebredEvent -> FilePath -> Text -> Int -> IO ()
checkForNewMail chan dbpath query delay = do
  r <- runExceptT (Notmuch.countThreads query dbpath)
  case r of
    Left _ -> pure ()
    Right n -> notify n *> rescheduleMailcheck chan dbpath query delay
  where
    notify = writeBChan chan . NotifyNewMailArrived

rescheduleMailcheck :: BChan PurebredEvent -> FilePath -> Text -> Int -> IO ()
rescheduleMailcheck chan dbpath query delay =
  void $ forkIO (threadDelay delay *> checkForNewMail chan dbpath query delay)

data StatusbarContext a
    = ListContext a
    | EditorContext a
    | ErrorContext a
    deriving (Show)

renderError :: Error -> Widget Name
renderError = withAttr statusbarErrorAttr . hCenter . theError
  where
    theError (GenericError e) = strWrap e
    theError e = strWrap (show e)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> renderError e
        Nothing ->
            case focusedViewWidget s of
                SearchThreadsEditor -> renderStatusbar (view (asMailIndex . miSearchThreadsEditor) s) s
                ManageMailTagsEditor -> renderStatusbar (view (asMailIndex . miMailTagsEditor) s) s
                ManageThreadTagsEditor -> renderStatusbar (view (asMailIndex . miThreadTagsEditor) s) s
                MailAttachmentOpenWithEditor -> renderStatusbar (view (asMailView . mvOpenCommand) s) s
                MailAttachmentPipeToEditor -> renderStatusbar (view (asMailView . mvPipeCommand) s) s
                ScrollingMailViewFindWordEditor -> renderStatusbar (view (asMailView . mvFindWordEditor) s) s
                ListOfThreads -> renderStatusbar (view (asMailIndex . miThreads) s) s
                ListOfMails -> renderStatusbar (view (asMailIndex . miMails) s) s
                ScrollingMailView -> renderStatusbar (view (asMailIndex . miMails) s) s
                ComposeListOfAttachments -> renderStatusbar (views (asCompose . cAttachments) lwl s) s
                MailListOfAttachments -> renderStatusbar (views (asMailView . mvAttachments) lwl s) s
                ListOfFiles -> renderStatusbar (views (asFileBrowser . fbEntries) lwl s) s
                ComposeTo -> renderStatusbar (view (asCompose . cTo) s) s
                ComposeFrom -> renderStatusbar (view (asCompose . cFrom) s) s
                ComposeSubject -> renderStatusbar (view (asCompose . cSubject) s) s
                _ -> withAttr statusbarAttr $ str "Purebred: " <+> fillLine

class WithContext a where
  renderContext :: AppState -> a -> Widget Name

instance WithContext (ListWithLength t e) where
  renderContext _ = currentItemW

instance WithContext (E.Editor Text Name) where
  renderContext _ = str . show . cursorPosition . view E.editContentsL

renderStatusbar :: WithContext w => w -> AppState -> Widget Name
renderStatusbar w s = withAttr statusbarAttr $ hBox
  [ str "Purebred: "
  , renderContext s w
  , renderNewMailIndicator s
  , renderMatches s
  , fillLine
  , txt (
      titleize (focusedViewName s) <> "-"
      <> titleize (focusedViewWidget s) <> " "
      )
  ]

renderMatches :: AppState -> Widget n
renderMatches s =
  let showCount = show $ matchCount $ view (asMailView . mvBody) s
      currentItem = view (non "0")
        $ preview (asMailView . mvScrollSteps . to focusGetCurrent . _Just . stNumber . to show) s
   in if has (asMailView . mvScrollSteps) s
        then str (currentItem <> " of " <> showCount <> " matches")
        else emptyWidget

renderNewMailIndicator :: AppState -> Widget n
renderNewMailIndicator s =
  let newMailCount = view (asMailIndex . miNewMail) s
      indicator = str $ "[New: " <> show newMailCount <> "]"
   in padLeftRight 1 indicator

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
