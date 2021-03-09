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

module UI.Mail.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types
import qualified Brick.Types as T

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KBS []) (scrollPageUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar '*') []) (toggleListItem `chain` listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollPageDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `focus` continue @'ViewMail @'ManageMailTagsEditor)

    , Keybinding (V.EvKey V.KUp []) (
        noop `focus` listUp @'ViewMail @'ListOfMails `focus` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (
        noop `focus` listDown @'ViewMail @'ListOfMails `focus` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `focus` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'J') []) (noop
                                             `focus` listDown @'Threads @'ListOfThreads
                                             `focus` displayThreadMails
                                             `focus` selectNextUnread
                                             `focus` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `focus` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'K') []) (noop
                                             `focus` listUp @'Threads @'ListOfThreads
                                             `focus` displayThreadMails
                                             `focus` selectNextUnread
                                             `focus` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `focus` continue @'Help @'ScrollingHelpView)
    , Keybinding (V.EvKey (V.KChar 'r') []) (
        senderReply
        `focus` (
            invokeEditor ViewMail ScrollingMailView
            :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
            )
        )
    , Keybinding (V.EvKey (V.KChar 'g') []) (
        groupReply
        `focus` (
            invokeEditor ViewMail ScrollingMailView
            :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
            )
        )
    , Keybinding (V.EvKey (V.KChar 'v') []) (noop `focus` continue @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'e') []) (composeAsNew `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '/') []) (noop `focus` continue @'ViewMail @'ScrollingMailViewFindWordEditor)
    , Keybinding (V.EvKey (V.KChar 'n') []) (scrollNextWord `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'f') []) (noop
                                             `chain` encapsulateMail
                                             `focus` continue @'ViewMail @'ComposeTo)
    , Keybinding (V.EvKey V.KEnter []) (removeHighlights `chain` continue)
    ]

findWordEditorKeybindings :: [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
findWordEditorKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'ScrollingMailView)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'ScrollingMailView)
  , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ViewMail @'ScrollingMailView)
  ]


mailAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
mailAttachmentsKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `focus` continue @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) openAttachment
    , Keybinding (V.EvKey (V.KChar 'o') []) (noop `focus` continue @'ViewMail @'MailAttachmentOpenWithEditor)
    , Keybinding (V.EvKey (V.KChar '|') []) (noop `focus` continue @'ViewMail @'MailAttachmentPipeToEditor)
    , Keybinding (V.EvKey (V.KChar 's') []) (noop `focus` continue @'ViewMail @'SaveToDiskPathEditor)
    ]

openWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
openWithKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `focus` continue @'ViewMail @'MailAttachmentPipeToEditor)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` openWithCommand)
    ]

pipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
pipeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `focus` continue @'ViewMail @'MailAttachmentOpenWithEditor)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` pipeToCommand)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` untoggleListItems @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'ScrollingMailView)
    ]

saveToDiskKeybindings :: [Keybinding 'ViewMail 'SaveToDiskPathEditor]
saveToDiskKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'MailListOfAttachments)
  , Keybinding (V.EvKey V.KEnter []) (
      done `chain` saveAttachmentToPath `focus` continue @'ViewMail @'MailListOfAttachments )
  ]

mailviewComposeToKeybindings :: [Keybinding 'ViewMail 'ComposeTo]
mailviewComposeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ViewMail @'ScrollingMailView)
    , Keybinding (V.EvKey V.KEnter []) (
        done `focus` (
            invokeEditor ViewMail ScrollingMailView
            :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
            )
        )
    ]
