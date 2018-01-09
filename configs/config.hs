-- This file is part of purebred
-- Copyright (C) 2017 Róman Joost
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
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Example configuration, currently used for testing which demonstrates various
ways to overwrite the configuration.
-}
import Purebred
import qualified Data.ByteString.Lazy as L
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromMaybe)
import Data.List (union)

myBrowseThreadsKbs :: [Keybinding 'Threads 'ListOfThreads]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
  ]

myBrowseMailKeybindings :: [Keybinding 'Mails 'ListOfMails]
myBrowseMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
    ]

myMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
    ]

writeMailtoFile :: Mail -> IO ()
writeMailtoFile m = do
  m' <- renderMail' m
  confdir <- lookupEnv "PUREBRED_CONFIG_DIR"
  currentdir <- getCurrentDirectory
  let fname = fromMaybe currentdir confdir </> "sentMail"
  L.writeFile fname m'


main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
    . over (confIndexView . ivBrowseMailsKeybindings) (`union` myBrowseMailKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
    . set (confComposeView . cvSendMailCmd) writeMailtoFile
    . set (confFileBrowserView . bfHomePath) getCurrentDirectory
