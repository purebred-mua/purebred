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
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromMaybe)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty(..))

myBrowseThreadsKbs :: [Keybinding 'Threads 'ListOfThreads]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"]
                                       `chain` untoggleListItems
                                       `chain` continue)
  ]

myMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"]
                                         `chain` untoggleListItems
                                         `chain` continue)
    ]

writeMailtoFile :: B.Builder -> IO (Either Error ())
writeMailtoFile m = do
  confdir <- lookupEnv "PUREBRED_CONFIG_DIR"
  currentdir <- getCurrentDirectory
  let fname = fromMaybe currentdir confdir </> "sentMail"
  L.writeFile fname (B.toLazyByteString m)
  pure (Right ())

fromMail :: [Mailbox]
fromMail =
    [ Mailbox
          (Just "Joe Bloggs")
          (AddrSpec "joe" (DomainDotAtom $ "foo" :| ["test"]))
    ]

main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
    . set (confComposeView . cvSendMailCmd) writeMailtoFile
    . set (confFileBrowserView . fbHomePath) getCurrentDirectory
    . set (confComposeView . cvIdentities) fromMail
    . over confTheme (applyAttrMappings myColoredTags)

myColoredTags :: [(AttrName, Attr)]
myColoredTags =
  [ (mailTagAttr <> "inbox", fg cyan)
  , (mailTagAttr <> "archive", fg cyan)
  , (mailTagAttr <> "signed", fg green)
  , (mailTagAttr <> "attachment", fg brightRed)
  ]
