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

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollPageUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollPageDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` focus @'ViewMail @'ManageMailTagsEditor `chain` continue)

    , Keybinding (V.EvKey V.KUp []) (noop `chain'` focus @'ViewMail @'ListOfMails `chain` listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (noop `chain'` focus @'ViewMail @'ListOfMails `chain` listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'J') []) (noop
                                             `chain'` listDown @'Threads @'ListOfThreads
                                             `chain'` displayThreadMails
                                             `chain'` selectNextUnread
                                             `chain'` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'K') []) (noop
                                             `chain'` listUp @'Threads @'ListOfThreads
                                             `chain'` displayThreadMails
                                             `chain'` selectNextUnread
                                             `chain'` displayMail
                                             `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `chain'` focus @'Help @'ScrollingHelpView `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'r') []) (replyMail `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` invokeEditor)
    , Keybinding (V.EvKey (V.KChar 'v') []) (noop `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'e') []) (composeAsNew `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar '/') []) (noop `chain'` focus @'ViewMail @'ScrollingMailViewFindWordEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'n') []) (scrollNextWord `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'f') []) (noop
                                             `chain` encapsulateMail
                                             `chain'` focus @'ViewMail @'ComposeTo
                                             `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (removeHighlights `chain` continue)
    ]

findWordEditorKeybindings :: [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
findWordEditorKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
  , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
  ]


mailAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
mailAttachmentsKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) openAttachment
    , Keybinding (V.EvKey (V.KChar 'o') []) (noop `chain'` focus @'ViewMail @'MailAttachmentOpenWithEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar '|') []) (noop `chain'` focus @'ViewMail @'MailAttachmentPipeToEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar 's') []) (noop `chain'` focus @'ViewMail @'SaveToDiskPathEditor `chain` continue)
    ]

openWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
openWithKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `chain'` focus @'ViewMail @'MailAttachmentPipeToEditor `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ViewMail @'MailListOfAttachments `chain` openWithCommand)
    ]

pipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
pipeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (abort `chain'` focus @'ViewMail @'MailAttachmentOpenWithEditor `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ViewMail @'MailListOfAttachments `chain` pipeToCommand)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    ]

saveToDiskKeybindings :: [Keybinding 'ViewMail 'SaveToDiskPathEditor]
saveToDiskKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'MailListOfAttachments `chain` continue)
  , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ViewMail @'MailListOfAttachments `chain` saveAttachmentToPath `chain` continue)
  ]

mailviewComposeToKeybindings :: [Keybinding 'ViewMail 'ComposeTo]
mailviewComposeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ViewMail @'ScrollingMailView `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` invokeEditor)
    ]
