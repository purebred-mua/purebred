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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-

Example configuration, currently used for testing which demonstrates various
ways to overwrite the configuration.

-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO.Unsafe

import Purebred
import Purebred.Storage.AddressBook.MuttAliasFile

myBrowseThreadsKbs :: [Keybinding 'Threads 'ListOfThreads]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'a') [])
      (setTags [RemoveTag "inbox", AddTag "archive"] *> untoggleListItems)
  ]

myMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') [])
      (setTags [RemoveTag "inbox", AddTag "archive"] *> untoggleListItems)
    ]

sendFailRef :: IORef Bool
sendFailRef = unsafePerformIO $ newIORef True
{-# NOINLINE sendFailRef #-}

writeMailtoFile :: B.Builder -> IO (Either Error ())
writeMailtoFile m = do
  sendFail <- lookupEnv "PUREBRED_SEND_FAIL" >>= \case
    Just (_:_)  -> readIORef sendFailRef
    _           -> pure False
  if sendFail
    then do
      writeIORef sendFailRef False
      pure . Left $ SendMailError "PUREBRED_SEND_FAIL"
    else do
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
main = do
  confdir <- lookupEnv "PUREBRED_CONFIG_DIR"
  cwd <- getCurrentDirectory

  let addrsFile = fromMaybe cwd confdir <> "/aliases"
  addressBook <-
    initMuttAliasFileAddressBook addrsFile
    >>= either (die . show) pure

  purebred
    [ usePlugin . tweakConfig $
        over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
        . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
        . set (confComposeView . cvSendMailCmd) writeMailtoFile
        . set (confComposeView . cvIdentities) fromMail
        . over confTheme (applyAttrMappings myColoredTags)
        . set (confIndexView . ivTagReplacementMap) tagReplacementMapAscii
        . set confAddressBook [addressBook]
    , usePlugin $ tweakConfigWithIO $ \conf -> do
        pure $ set (confFileBrowserView . fbHomePath) cwd conf
    ]

myColoredTags :: [(AttrName, Attr)]
myColoredTags =
  [ (mailTagAttr <> attrName "inbox", fg cyan)
  , (mailTagAttr <> attrName "archive", fg cyan)
  , (mailTagAttr <> attrName "signed", fg green)
  , (mailTagAttr <> attrName "attachment", fg brightRed)
  ]
