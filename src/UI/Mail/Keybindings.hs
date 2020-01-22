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

module UI.Mail.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollPageUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollPageDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ViewMail 'ManageMailTagsEditor AppState) `chain` continue)

    , Keybinding (V.EvKey V.KUp []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'J') []) (noop
                                             `chain'` (listDown :: Action 'Threads 'ListOfThreads AppState)
                                             `chain'` displayThreadMails
                                             `chain'` selectNextUnread
                                             `chain'` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'K') []) (noop
                                             `chain'` (listUp :: Action 'Threads 'ListOfThreads AppState)
                                             `chain'` displayThreadMails
                                             `chain'` selectNextUnread
                                             `chain'` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `chain'` (focus :: Action 'Help 'ScrollingHelpView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'r') []) (replyMail `chain'` (focus :: Action 'ComposeView 'ComposeListOfAttachments AppState) `chain` invokeEditor)
    , Keybinding (V.EvKey (V.KChar 'v') []) (noop `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'e') []) (composeAsNew `chain'` (focus :: Action 'ComposeView 'ComposeListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '/') []) (noop `chain'` (focus :: Action 'ViewMail 'ScrollingMailViewFindWordEditor AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'n') []) (scrollNextWord `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'f') []) (noop
                                             `chain` encapsulateMail
                                             `chain'` (focus :: Action 'ViewMail 'ComposeTo AppState)
                                             `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (removeHighlights `chain` continue)
    ]

findWordEditorKeybindings :: [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
findWordEditorKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
  , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
  ]


mailAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
mailAttachmentsKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) openAttachment
    , Keybinding (V.EvKey (V.KChar 'o') []) (noop `chain'` (focus :: Action 'ViewMail 'MailAttachmentOpenWithEditor AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '|') []) (noop `chain'` (focus :: Action 'ViewMail 'MailAttachmentPipeToEditor AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 's') []) (noop `chain'` (focus :: Action 'ViewMail 'SaveToDiskPathEditor AppState) `chain` continue)
    ]

openWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
openWithKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `chain'` (focus :: Action 'ViewMail 'MailAttachmentPipeToEditor AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` openWithCommand)
    ]

pipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
pipeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `chain'` (focus :: Action 'ViewMail 'MailAttachmentOpenWithEditor AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` pipeToCommand)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    ]

saveToDiskKeybindings :: [Keybinding 'ViewMail 'SaveToDiskPathEditor]
saveToDiskKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` continue)
  , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'MailListOfAttachments AppState) `chain` saveAttachmentToPath `chain` continue)
  ]

mailviewComposeToKeybindings :: [Keybinding 'ViewMail 'ComposeTo]
mailviewComposeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ComposeView 'ComposeListOfAttachments AppState) `chain` invokeEditor)
    ]
