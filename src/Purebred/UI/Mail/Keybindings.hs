-- This file is part of purebred
-- Copyright (C) 2017-2020 Róman Joost and Fraser Tweedale
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Purebred.UI.Mail.Keybindings where

import qualified Graphics.Vty as V
import Purebred.UI.Actions
import Purebred.Types

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KBS []) scrollPageUp
    , Keybinding (V.EvKey (V.KChar '*') []) (toggleListItem *> listDown)
    , Keybinding (V.EvKey (V.KChar 't') []) setUnread
    , Keybinding (V.EvKey (V.KChar ' ') []) scrollPageDown
    , Keybinding (V.EvKey (V.KChar 'h') []) toggleHeaders
    , Keybinding (V.EvKey (V.KChar '`') []) (switchView @'ViewMail @'ManageMailTagsEditor)

    , Keybinding (V.EvKey V.KUp [])
        (switchView @'ViewMail @'ListOfMails *> listUp *> displayMail)
    , Keybinding (V.EvKey V.KDown [])
        (switchView @'ViewMail @'ListOfMails *> listDown *> displayMail)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown !*> displayMail)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp !*> displayMail)
    , Keybinding (V.EvKey (V.KChar 'J') [])
        ( noop
          !*> listDown @'Threads @'ListOfThreads
          !*> displayThreadMails
          !*> selectNextUnread
          !*> displayMail
        )
    , Keybinding (V.EvKey (V.KChar 'K') [])
        ( noop
          !*> listUp @'Threads @'ListOfThreads
          !*> displayThreadMails
          !*> selectNextUnread
          !*> displayMail
        )
    , Keybinding (V.EvKey (V.KChar '?') []) (switchView @'Help @'ScrollingHelpView)
    , Keybinding (V.EvKey (V.KChar 'r') []) (
        senderReply
        `focus` (
            invokeEditor ViewMail ScrollingMailView
            :: Action 'ComposeView 'ComposeListOfAttachments ()
            )
        )
    , Keybinding (V.EvKey (V.KChar 'g') []) (
        groupReply
        `focus` (
            invokeEditor ViewMail ScrollingMailView :: Action 'ComposeView 'ComposeListOfAttachments ()
            )
        )
    , Keybinding (V.EvKey (V.KChar 'v') [])
        (switchView @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'e') [])
        (composeAsNew *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '/') [])
        (switchView @'ViewMail @'ScrollingMailViewFindWordEditor)
    , Keybinding (V.EvKey (V.KChar 'n') [])
        scrollNextWord
    , Keybinding (V.EvKey (V.KChar 'f') [])
        (encapsulateMail *> switchView @'ViewMail @'ComposeTo)
    , Keybinding (V.EvKey V.KEnter [])
        removeHighlights
    ]

findWordEditorKeybindings :: [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
findWordEditorKeybindings =
  [ Keybinding (V.EvKey V.KEsc [])
      (abort *> switchView @'ViewMail @'ScrollingMailView)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
      (abort *> switchView @'ViewMail @'ScrollingMailView)
  , Keybinding (V.EvKey V.KEnter [])
      (done *> switchView @'ViewMail @'ScrollingMailView)
  ]


mailAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
mailAttachmentsKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'j') []) listDown
    , Keybinding (V.EvKey (V.KChar 'k') []) listUp
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort *> switchView @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) openAttachment
    , Keybinding (V.EvKey (V.KChar 'o') []) (switchView @'ViewMail @'MailAttachmentOpenWithEditor)
    , Keybinding (V.EvKey (V.KChar '|') []) (switchView @'ViewMail @'MailAttachmentPipeToEditor)
    , Keybinding (V.EvKey (V.KChar 's') []) (switchView @'ViewMail @'SaveToDiskPathEditor)
    ]

openWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
openWithKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort *> switchView @'ViewMail @'MailAttachmentPipeToEditor)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` openWithCommand)
    ]

pipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
pipeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort *> switchView @'ViewMail @'MailAttachmentOpenWithEditor)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` pipeToCommand)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` untoggleListItems @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'ViewMail @'ScrollingMailView)
    ]

saveToDiskKeybindings :: [Keybinding 'ViewMail 'SaveToDiskPathEditor]
saveToDiskKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'ViewMail @'MailListOfAttachments)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'ViewMail @'MailListOfAttachments)
  , Keybinding (V.EvKey V.KEnter [])
      ( done
        *> saveAttachmentToPath
        *> switchView @'ViewMail @'MailListOfAttachments
      )
  ]

mailviewComposeToKeybindings :: [Keybinding 'ViewMail 'ComposeTo]
mailviewComposeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) (
        done `focus` (
            invokeEditor ViewMail ScrollingMailView
            :: Action 'ComposeView 'ComposeListOfAttachments ()
            )
        )
    ]
