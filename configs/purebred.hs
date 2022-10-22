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

{-

Example configuration, currently used for testing which demonstrates various
ways to overwrite the configuration.

-}

import Control.Monad.IO.Class (liftIO)
import Purebred
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty(..))

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
  sendFail <- lookupEnv "PUREBRED_SEND_FAIL" >>= \v -> case v of
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
main =
  purebred
    [ usePlugin $ tweakConfig tweak
    , usePlugin $ tweakConfigWithIO $ \conf -> do
        cwd <- liftIO getCurrentDirectory
        pure $ set (confFileBrowserView . fbHomePath) cwd conf
    ]
  where
  tweak =
    over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
    . set (confComposeView . cvSendMailCmd) writeMailtoFile
    . set (confComposeView . cvIdentities) fromMail
    . over confTheme (applyAttrMappings myColoredTags)
    . set (confIndexView . ivTagReplacementMap) tagReplacementMapAscii

myColoredTags :: [(AttrName, Attr)]
myColoredTags =
  [ (mailTagAttr <> attrName "inbox", fg cyan)
  , (mailTagAttr <> attrName "archive", fg cyan)
  , (mailTagAttr <> attrName "signed", fg green)
  , (mailTagAttr <> attrName "attachment", fg brightRed)
  ]
