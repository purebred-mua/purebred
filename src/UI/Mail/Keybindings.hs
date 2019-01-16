{-# LANGUAGE DataKinds #-}

module UI.Mail.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollPageUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollPageDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ViewMail 'ManageMailTagsEditor AppState) `chain` continue)

    , Keybinding (V.EvKey V.KUp []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'J') []) (noop
                                             `chain'` (focus :: Action 'Threads 'ListOfThreads AppState)
                                             `chain` listDown
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
    ]

mailAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
mailAttachmentsKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    ]
