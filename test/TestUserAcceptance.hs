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

{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Char (chr)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Control.Concurrent (threadDelay)
import System.IO (hPutStr, stderr)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath.Posix ((</>))
import Control.Monad (filterM, void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT)

import Control.Lens (Getter, Lens', preview, to, view, _init, _last)
import System.Directory
  ( copyFile, getCurrentDirectory, listDirectory, removeDirectoryRecursive
  , removeFile
  )
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Process.Typed (proc, runProcess_, readProcess_, setEnv)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (assertBool)

import Data.MIME (parse, message, mime, MIMEMessage)

import UAT

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

type PurebredTestCase = TestCase GlobalEnv

main :: IO ()
main = defaultMain $ testTmux pre post tests
  where
    pre = do
      dir <- mkTempDir
      setUpPurebredConfig dir
      precompileConfig dir  -- all tests can use same precompiled binary
      pure (GlobalEnv dir)

    post (GlobalEnv dir) =
      removeDirectoryRecursive dir

    tests =
      [ testUserViewsMailSuccessfully
      , testUserCanManipulateNMQuery
      , testUserCanSwitchBackToIndex
      , testUserCanAbortMailComposition
      , testSendMail
      , testCanToggleHeaders
      , testSetsMailToRead
      , testShowsAndClearsError
      , testHelp
      , testManageTagsOnMails
      , testManageTagsOnThreads
      , testConfig
      , testUpdatesReadState
      , testCanJumpToFirstListItem
      , testAddAttachments
      , testFromAddressIsProperlyReset
      , testRepliesToMailSuccessfully
      , testUserCanMoveBetweenThreads
      , testShowsMailEntities
      , testOpenCommandDoesNotKillPurebred
      , testOpenEntitiesSuccessfully
      , testPipeEntitiesSuccessfully
      , testEditingMailHeaders
      , testShowsInvalidCompositionInput
      , testShowsInvalidTaggingInput
      ]

testShowsInvalidTaggingInput :: PurebredTestCase
testShowsInvalidTaggingInput = purebredTmuxSession "shows errors when tagging" $
  \step -> do
    startApplication

    step "start tagging"
    sendKeys "`" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] []))

    step "enter invalid tag input"
    sendKeys "=," (Substring "Failed reading: unexpected ',' at offset 1")

    step "clear"
    sendKeys "BSpace" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] [] <> "="))

    step "exit editor"
    sendKeys "C-g" (Substring "Query")

    step "open thread"
    sendKeys "Enter" (Substring "Testmail with whitespace")

    step "start tagging"
    sendKeys "`" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] []))

    step "enter invalid tag input"
    sendKeys "=," (Substring "Failed reading: unexpected ',' at offset 1")

    step "clear"
    sendKeys "BSpace" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] [] <> "="))

testShowsInvalidCompositionInput :: PurebredTestCase
testShowsInvalidCompositionInput = purebredTmuxSession "shows errors when composing" $
  \step -> do
    startApplication
    step "start composition"
    sendKeys "m" (Substring "From")

    step "trigger error"
    sendKeys "<" (Substring "Failed reading")

    step "continue"
    sendKeys "BSpace" (Substring "Purebred: (0,27)")
    sendKeys "Enter" (Substring "To:")

    step "trigger error"
    sendKeys "," (Substring "Failed reading")

    step "continue"
    sendKeys "BSpace" (Substring "Purebred: (0,0)")
    sendKeys "Enter" (Substring "Subject:")

    step "leave empty subject"
    sendKeys "Enter" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendKeys ": x\r" (Substring "text/plain")

    step "focus from field"
    sendKeys "f" (Regex $ "From:\\s"
                  <> buildAnsiRegex [] ["37"] []
                  <> "\"Joe Bloggs\" <joe@foo.test>")

    step "trigger error"
    sendKeys "," (Substring "Failed reading: unexpected ',' at offset 27")

    step "abort editing"
    sendKeys "C-g" (Substring "ComposeView-Attachments")

    step "focus to field"
    sendKeys "t" (Regex $ "To:\\s" <> buildAnsiRegex [] ["37"] [])

    step "trigger error"
    sendKeys "," (Substring "Failed reading")

    step "abort editing"
    sendKeys "C-g" (Substring "ComposeView-Attachments")
  

testEditingMailHeaders :: PurebredTestCase
testEditingMailHeaders = purebredTmuxSession "user can edit mail headers" $
  \step -> do
    startApplication

    step "start composition"
    sendKeys "m" (Substring "From")

    step "accept default"
    sendKeys "Enter" (Substring "To")

    step "enter to: email"
    sendKeys "user@to.test\r" (Substring "Subject")

    step "leave default"
    sendKeys "Enter" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendKeys ": x\r" (Substring "text/plain")
      >>= assertSubstring "From: \"Joe Bloggs\" <joe@foo.test>"

    step "user can change from header"
    sendKeys "f" (Regex $ "From: " <> buildAnsiRegex [] ["37"] [] <> "\"Joe Bloggs\" <joe@foo.test>")

    step "append an email"
    sendKeys ", testuser@foo.test\r" (Substring $ "From: "
                                      <> "\"Joe Bloggs\" <joe@foo.test>, testuser@foo.test")

    step "user can change to header"
    sendKeys "t" (Regex $ "To: " <> buildAnsiRegex [] ["37"] [] <> "user@to.test")

    step "append an additional from email"
    sendKeys ", testuser@foo.test\r" (Substring "To: user@to.test, testuser@foo.test")

    step "change subject"
    sendKeys "s" (Regex $ "Subject: " <> buildAnsiRegex [] ["37"] [] <> "")

    step "enter subject"
    sendKeys "foo subject\r" (Substring "Subject: foo subject")

testPipeEntitiesSuccessfully :: PurebredTestCase
testPipeEntitiesSuccessfully = purebredTmuxSession "pipe entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "pipe to"
    sendKeys "|" (Substring "Pipe to")

    step "use less"
    sendLine "less" (Regex ("This is a test mail for purebred"
                             <> buildAnsiRegex [] ["37"] ["40"]
                             <> "\\s+"
                             <> buildAnsiRegex ["7"] ["39"] ["49"]
                             <> "\\(END\\)"))

testOpenEntitiesSuccessfully :: PurebredTestCase
testOpenEntitiesSuccessfully = purebredTmuxSession "open entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "open one entity"
    sendKeys "o" (Substring "Open With")
    sendLine "less" (Regex ("This is a test mail for purebred"
                            <> buildAnsiRegex [] ["37"] ["40"]
                            <> "\\s+"
                            <> buildAnsiRegex ["7"] ["39"] ["49"]
                            <> ".*purebred.*END"))

testOpenCommandDoesNotKillPurebred :: PurebredTestCase
testOpenCommandDoesNotKillPurebred = purebredTmuxSession "open attachment does not kill purebred" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "open with"
    sendKeys "o" (Substring "Open With")

    step "Open with bogus command"
    sendLine "asdfasdfasdf" (Substring "ProcessError")

testShowsMailEntities :: PurebredTestCase
testShowsMailEntities = purebredTmuxSession "shows mail entities successfully" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "select the second entity"
    sendKeys "j" (Substring "text/html")

    step "close the list of entities"
    out <- sendKeys "q" (Substring "This is a test mail for purebred")

    -- poor mans (?!text)
    assertRegex "[^t][^e][^x][^t]" out

testUserCanMoveBetweenThreads :: PurebredTestCase
testUserCanMoveBetweenThreads = purebredTmuxSession "user can navigate between threads" $
  \step -> do
    startApplication
    -- assert that the first mail is really the one we're later navigating back
    -- to
    snapshot
    assertRegexS (buildAnsiRegex ["1"] ["37"] ["43"] <> "\\sAug'17.*Testmail with whitespace")

    step "View Mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "Navigate down the threads list"
    sendKeys "J" (Substring "HOLY PUREBRED")

    step "Navigate up the threads list"
    sendKeys "K" (Substring "This is a test mail for purebred")

testRepliesToMailSuccessfully :: PurebredTestCase
testRepliesToMailSuccessfully = purebredTmuxSession "replies to mail successfully" $
  \step -> do
    let subject = "Testmail with whitespace in the subject"
    testdir <- view effectiveDir
    startApplication

    step "pick first mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred") >>= put

    assertSubstringS "From: <roman@host.example>"
    assertSubstringS "To: <frase@host.example>"
    assertSubstringS ("Subject: " <> subject)

    step "start replying"
    sendKeys "r" (Substring "> This is a test mail for purebred")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments") >>= put

    assertSubstringS "From: <frase@host.example>"
    assertSubstringS "To: <roman@host.example>"
    assertSubstringS ("Subject: Re: " <> subject)

    step "send mail"
    sendKeys "y" (Substring "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr ("Subject: Re: " <> subject) decoded
    assertSubstr "From: frase@host.example" decoded
    assertSubstr "To: roman@host.example" decoded
    assertSubstr "> This is a test mail for purebred" decoded

testFromAddressIsProperlyReset :: PurebredTestCase
testFromAddressIsProperlyReset = purebredTmuxSession "from address is reset to configured identity" $
  \step -> do
    startApplication

    step "Start composing"
    sendKeys "m" (Substring "Joe Bloggs")

    step "abort editing"
    sendKeys "Escape" (Substring "tag:inbox")

    step "Start composing again"
    sendKeys "m" (Substring "Joe Bloggs")

testCanJumpToFirstListItem :: PurebredTestCase
testCanJumpToFirstListItem = purebredTmuxSession "can jump to first and last mail" $
  \step -> do
    startApplication

    step "Jump to last mail"
    sendKeys "G" (Substring "3 of 3")

    step "Jump to first mail"
    sendKeys "1" (Substring "1 of 3")

testUpdatesReadState :: PurebredTestCase
testUpdatesReadState = purebredTmuxSession "updates read state for mail and thread" $
  \step -> do
    startApplication

    step "navigate to thread mails"
    sendKeys "G" (Substring "3 of 3")

    step "view unread mail in thread"
    sendKeys "Enter" (Substring "WIP Refactor")

    step "view next unread in thread"
    sendKeys "Down" (Substring "2 of 2")

    step "go back to thread list which is read"
    sendKeys "q q" (Regex (buildAnsiRegex [] ["37"] ["43"] <> " Feb'17\\sRóman\\sJoost\\s+\\(2\\)"))

    step "set one mail to unread"
    sendKeys "Enter" (Substring "Beginning of large text")
    sendKeys "q t" (Regex (buildAnsiRegex ["1"] ["37"] []
                           <> "\\sRe: WIP Refactor\\s+"
                           <> buildAnsiRegex ["0"] ["34"] ["40"]))

    step "returning to thread list shows thread unread"
    sendKeys "q" (Regex (buildAnsiRegex ["1"] ["37"] [] <> "\\sWIP Refactor\\s"))

testConfig :: PurebredTestCase
testConfig = purebredTmuxSession "test custom config" $
  \step -> do
    -- Set a short command prompt, to a value otherwise unlikely to
    -- appear, so that we can easily check for program termination.
    let unlikelyString = "unlikely"
    sendKeys ("PS1=" <> unlikelyString <> "$ \r") (Substring unlikelyString)
    startApplication

    step "archive thread"
    sendKeys "a" (Substring "archive")

    step "quit"
    sendKeys "q" Unconditional

    -- Wait a bit so that purebred, which may not yet have
    -- terminated, does not eat the upcoming keystroke(s)
    liftIO $ threadDelay 200000  -- 0.2 seconds

    -- Press Enter again to deal with case where cursor is not at
    -- column 0, which could cause target string to be split.
    sendKeys "Enter" (Substring unlikelyString)

testAddAttachments :: PurebredTestCase
testAddAttachments = purebredTmuxSession "use file browser to add attachments" $
  \step -> do
    testdir <- view effectiveDir

    -- To be resilient against differences in list contents between
    -- git and sdist, list the directory ourselves to work out what
    -- the final entry should be.  Note that dirs come first in the
    -- filebrowser widget.
    files <- fmap sort $ liftIO $
      getSourceDirectory >>= listDirectory
      >>= filterM (fmap isRegularFile . getFileStatus)
    let
      lastFile = fromMaybe "MISSING" $ preview _last files
      secondLastFile = fromMaybe "MISSING" $ preview (_init . _last) files

    startApplication

    step "start composition"
    sendKeys "m" (Substring "From")

    step "enter from email"
    sendKeys "Enter" (Substring "To")

    step "enter to: email"
    sendKeys "user@to.test\r" (Substring "Subject")

    step "enter subject"
    sendKeys "test subject\r" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments")

    step "start file browser"
    cwd <- liftIO getCurrentDirectory
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] ["40"] <> cwd)

    step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - " <> lastFile)

    step "add first selected file"
    sendKeys "Enter" (Substring lastFile)

    step "up to select mail body"
    sendKeys "Up" (Substring "Item 1 of 2")

    -- edit the mail body a few times to check if the code not mistakenly adds
    -- the same mail body as an attachment
    step "edit mail body text"
    sendKeys "e" (Substring "test body")

    step "append to mail body"
    sendKeys "i. foo" (Substring "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "foo")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments")

    step "edit mail body text"
    sendKeys "e" (Substring "test body")

    step "append to mail body"
    sendKeys "i. foo" (Substring "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "foo")

    step "exit vim"
    sendKeys ": x\r" (Substring "Item 1 of 2")

    -- try removing attachments
    step "select the attachment"
    sendKeys "Down" (Substring "Item 2 of 2")

    step "remove the attachment"
    sendKeys "D" (Substring "Item 1 of 1")

    step "try to remove the last attachment"
    sendKeys "D" (Substring "You may not remove the only attachment")

    -- add the attachment again and send it
    step "start file browser"
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] ["40"] <> cwd)

    step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - " <> lastFile)

    step "select the file"
    sendKeys "Space" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9745 - " <> lastFile)

    step "move one item up"
    sendKeys "Up" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - " <> secondLastFile)

    step "add selected files"
    out <- sendKeys "Enter" (Substring "Item 3 of 3")
    assertSubstring secondLastFile out

    step "send mail"
    sendKeys "y" (Substring "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr "attachment; filename" decoded
    assertSubstr secondLastFile decoded
    assertSubstr lastFile decoded
    assertSubstr "This is a test body" decoded

testManageTagsOnMails :: PurebredTestCase
testManageTagsOnMails = purebredTmuxSession "manage tags on mails" $
  \step -> do
    startApplication

    step "view mail in thread"
    sendKeys "Enter" (Substring "Testmail")

    step "focus command to show mail tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter new tag"
    sendLine "+inbox +foo +bar" (Regex ("foo"
                             <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] []
                             <> "bar"))
      >>= assertSubstring "This is a test mail"

    step "go back to list of mails"
    sendKeys "Escape" (Substring "List of Mails")

    step "go back to list of threads"
    sendKeys "Escape" (Substring "List of Threads")

    -- find newly tagged mail
    step "focus tag search"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter tag to search `foo and bar`"
    sendLine "tag:foo and tag:bar" (Substring "tag:foo and tag:bar")

    step "view mail in thread"
    sendKeys "Enter" (Substring "Testmail")

    step "attempt to add a new tag"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "cancel tagging and expect old UI"
    -- instead of asserting the absence of the tagging editor, we assert the
    -- last visible "item" in the UI followed by whitespace.
    sendKeys "Escape" (Regex "This is a test mail for purebred\\s+$")

testManageTagsOnThreads :: PurebredTestCase
testManageTagsOnThreads = purebredTmuxSession "manage tags on threads" $
  \step -> do
    startApplication

    -- setup: tag the mails in the thread with two different tags and then
    -- tag the thread as a whole with a new tag. All mails should keep their
    -- distinct tags, while having received a new tag.
    step "navigate to thread"
    sendKeys "Down" (Substring "Item 2 of 3")
    sendKeys "Down" (Substring "Item 3 of 3")

    step "show thread mails"
    sendKeys "Enter" (Substring "ViewMail")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    sendLine "+archive" (Substring "archive")

    step "move to second mail"
    sendKeys "Down" (Substring "Item 2 of 2")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    sendLine "+replied -inbox" (Substring "replied")

    step "go back to list of mails"
    sendKeys "Escape" (Substring "Item 2 of 2")

    step "thread tags shows new tags"
    sendKeys "Escape" (Regex ("archive"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\s"
                              <> buildAnsiRegex [] ["36"] []
                              <> "replied"))

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "remove tag"
    -- "cheating" here a bit, since just invoking tmux with sending literally
    -- "-only" will fail due to tmux parsing it as an argument, but the mail is
    -- already tagged with "thread" so the additional adding won't do anything
    sendLine "+thread" (Regex ("archive"
                             <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "replied" <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "thread"))

    step "show thread mails"
    sendKeys "Enter" (Substring "ViewMail")

    step "second mail shows old tag"
    sendKeys "Escape" (Regex ("replied"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\s"
                              <> buildAnsiRegex [] ["36"] []
                              <> "thread"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\sRe: WIP Refactor"))

    step "go back to list of threads"
    sendKeys "Escape" (Substring "List of Threads")

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "abort editing"
    sendKeys "Escape" (Substring "Query")

testHelp :: PurebredTestCase
testHelp = purebredTmuxSession "help view" $
  \step -> do
    startApplication

    step "shows Keybindings"
    sendKeys "?" (Substring "quit the application")

    sendKeys "Escape" (Substring "Purebred")

testShowsAndClearsError :: PurebredTestCase
testShowsAndClearsError = purebredTmuxSession "shows and clears error" $
  \step -> do
    startApplication

    testmdir <- view envMaildir
    liftIO $ removeFile (testmdir <> "/Maildir/new/1502941827.R15455991756849358775.url")

    step "open thread"
    sendKeys "Enter" (Substring "Testmail")

    step "shows error message"
    sendKeys "Enter" (Substring "FileReadError")
      >>= assertRegex "open(Binary)?File:.*does not exist"

    step "error is cleared with next registered keybinding"
    sendKeys "Up" (Substring "Purebred: Item 1 of 3")

testSetsMailToRead :: PurebredTestCase
testSetsMailToRead = purebredTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "first unread mail is opened"
    sendKeys "Escape" (Substring "List of Mails")
      >>= assertRegex (buildAnsiRegex [] ["37"] ["43"] <> ".*Testmail")

    step "toggle it back to unread (bold again)"
    sendKeys "t" (Regex (buildAnsiRegex ["1"] ["37"] ["43"] <> ".*Testmail"))

testCanToggleHeaders :: PurebredTestCase
testCanToggleHeaders = purebredTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    step "open thread"
    sendKeys "Enter" (Substring "Testmail")

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail")

    step "toggle to show all headers"
    sendKeys "h" (Regex "[Rr]eturn-[Pp]ath")

    step "toggle filtered headers"
    out <- sendKeys "h" (Substring "This is a test mail")
    assertRegex "Purebred.*\n.*[Ff]rom" out

testUserViewsMailSuccessfully :: PurebredTestCase
testUserViewsMailSuccessfully = purebredTmuxSession "user can view mail" $
  \step -> do
    startApplication
    step "shows tag"
    snapshot
    assertSubstringS "inbox"
    assertSubstringS "Testmail with whitespace in the subject"

    step "open thread"
    sendKeys "Enter" (Substring "Testmail with whitespace in the subject")

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail")

    step "go back to thread list"
    sendKeys "q q" (Substring "WIP Refactor")

    step "Move down to threaded mails"
    sendKeys "Down" (Substring "Purebred: Item 2 of 3")
    sendKeys "Down" (Substring "Purebred: Item 3 of 3")
    sendKeys "Enter" (Substring "Re: WIP Refactor")

    step "Scroll down"
    sendKeys "Enter" (Substring "Beginning of large text")
    sendKeys "Space" (Substring "Sed ut perspiciatis")

    step "go to next unread mail"
    sendKeys "j" (Substring "Re: WIP Refactor")

    step "Scroll down (again)"
    sendKeys "Space" (Substring "Sed ut perspiciatis")

    step "go to previous mail with reset scroll state"
    sendKeys "k" (Regex "Subject:\\s.*WIP Refactor")

testUserCanManipulateNMQuery :: PurebredTestCase
testUserCanManipulateNMQuery =
   purebredTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          step "focus command"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

          step "delete all input"
          sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

          step "search for non existing tags yielding no results"
          sendLine "does not match anything" (Substring "No items")

          step "search for mail correctly tagged"
          sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "does"))
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

          step "enter new tag"
          sendLine "tag:replied" (Substring "Item 1 of 1")

          step "open thread"
          sendKeys "Enter" (Substring "This is Purebred")

          step "view currently selected mail"
          sendKeys "Enter" (Substring "HOLY PUREBRED")

testUserCanSwitchBackToIndex :: PurebredTestCase
testUserCanSwitchBackToIndex =
  purebredTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            step "start composition"
            sendKeys "m" (Substring "From")

            step "enter from email"
            sendKeys "C-a" Unconditional
            sendKeys "C-k" Unconditional
            sendKeys "testuser@foo.test\r" (Substring "To")

            step "enter to: email"
            sendKeys "user@to.test\r" (Substring "Subject")

            step "enter subject"
            sendKeys "test subject\r" (Substring "~")

            step "enter mail body"
            sendKeys "iThis is a test body" (Substring "body")

            step "exit insert mode in vim"
            sendKeys "Escape" (Substring "body")

            step "exit vim"
            sendKeys ": x\r" (Regex "From: testuser@foo.test")

            step "switch back to index"
            sendKeys "Tab" (Substring "Testmail")

            step "switch back to the compose editor"
            sendKeys "Tab" (Substring "test subject")

testUserCanAbortMailComposition :: PurebredTestCase
testUserCanAbortMailComposition =
  purebredTmuxSession "user can abort composing mail" $
        \step -> do
            startApplication
            step "start composition"
            sendKeys "m" (Substring "From")

            step "enter from email"
            sendKeys "Enter" (Substring "To")

            step "enter to: email"
            sendKeys "user@to.test\r" (Substring "Subject")

            step "enter subject"
            sendKeys "test subject\r" (Substring "~")

            step "enter mail body"
            sendKeys "iThis is a test body" (Substring "body")

            step "exit insert mode in vim"
            sendKeys "Escape" (Substring "body")

            step "exit vim"
            sendKeys ": x\r" (Regex $ "From: "
                             <> "\"Joe Bloggs\" <joe@foo.test>")

            step "abort mail"
            sendKeys "q" (Substring "Keep draft?")

            step "choose discard"
            -- TODO: buildAnsiRegex will cause the generated Regex not
            -- to match. Maybe not \\s+ even though raw it looks like
            -- there is white space?
            -- see https://github.com/purebred-mua/tasty-tmux/issues/8
            sendKeys "Tab" (Substring "Discard")

            step "confirm discard"
            sendKeys "Enter" (Substring "Testmail")

            step "start composition again"
            sendKeys "m" (Substring "From")
            sendKeys "Enter" (Regex ("To:\\s" <> buildAnsiRegex [] ["37"] []))

            step "enter to: email"
            sendKeys "new@second.test\r" (Regex ("Subject:\\s" <> buildAnsiRegex [] ["37"] []))

            step "enter subject"
            sendKeys "test subject\r" (Regex "~\\s+")

            step "enter mail body"
            sendKeys "iThis is my second mail" Unconditional

            step "exit insert mode in vim"
            sendKeys "Escape" Unconditional

            step "exit vim"
            sendKeys ": x\r" (Regex ("To: new@second.test\\s+"
                                     <> "Subject: test subject"))

            step "edit body"
            sendKeys "e" (Regex "This is my second mail\\s+")


testSendMail :: PurebredTestCase
testSendMail =
  purebredTmuxSession "sending mail successfully" $
        \step -> do
          testdir <- view effectiveDir
          startApplication

          step "start composition"
          sendKeys "m" (Substring "From")

          step "append an additional from email"
          sendKeys ", testuser@foo.test\r" (Substring "To")

          step "enter to: email"
          sendKeys "user@to.test\r" (Substring "Subject")

          step "enter subject"
          let subj = "test subject from directory " <> testdir
          sendKeys (subj <> "\r") (Substring "~")

          step "enter mail body"
          sendKeys "iThis is a test body" (Substring "body")

          step "exit insert mode in vim"
          sendKeys "Escape" (Substring "body")

          step "exit vim"
          sendKeys ": x\r" (Substring "text/plain")
            >>= assertRegex ("From: "
                             <> "\"Joe Bloggs\" <joe@foo.test>, testuser@foo.test")

          step "user can re-edit body"
          sendKeys "e" (Substring "This is a test body")

          step "Writes more text"
          sendKeys "i. More text" (Substring "text")

          step "exit insert mode in vim"
          sendKeys "Escape" (Substring "body")

          step "exit vim"
          sendKeys ": x\r" (Regex ("text/plain\\s" <> buildAnsiRegex [] ["34"] ["40"] <> "\\s+"))

          step "send mail and go back to threads"
          sendKeys "y" (Regex ("Query:\\s" <> buildAnsiRegex [] ["34"] [] <> "tag:inbox"))

          step "parse mail with purebred-email"
          assertMailSuccessfullyParsed (testdir </> "sentMail")


parseMail :: B.ByteString -> Either String MIMEMessage
parseMail = parse (message mime)

assertMailSuccessfullyParsed :: (MonadIO m) => String -> m ()
assertMailSuccessfullyParsed fp = do
  contents <- liftIO $ B.readFile fp
  let result = parseMail contents
  liftIO $ assertBool "expected successful MIMEMessage" (isRight result)

assertSubstr :: MonadIO m => String -> String -> m ()
assertSubstr needle haystack = liftIO $ assertBool
  (needle <> " not found in\n\n" <> haystack)
  (needle `isInfixOf` haystack)

-- Global test environment (shared by all test cases)
newtype GlobalEnv = GlobalEnv FilePath

globalEnvDir :: Lens' GlobalEnv FilePath
globalEnvDir f (GlobalEnv a) = fmap GlobalEnv (f a)

-- Session test environment
data Env = Env
  { _envGlobalEnv :: GlobalEnv
  , _envDir :: Maybe FilePath   -- override the global config dir
  , _envMaildir :: FilePath
  , _envSessionName :: String
  }

instance HasTmuxSession Env where
  tmuxSession = envSessionName

globalEnv :: Lens' Env GlobalEnv
globalEnv f (Env a b c d) = fmap (\a' -> Env a' b c d) (f a)

sessionEnvDir :: Lens' Env (Maybe FilePath)
sessionEnvDir f (Env a b c d) = fmap (\b' -> Env a b' c d) (f b)

-- | The effective config dir for a session
effectiveDir :: Getter Env FilePath
effectiveDir = to $ \env ->
  fromMaybe (view (globalEnv . globalEnvDir) env) (view sessionEnvDir env)

envMaildir :: Lens' Env FilePath
envMaildir f (Env a b c d) = fmap (\c' -> Env a b c' d) (f c)

envSessionName :: Lens' Env String
envSessionName f (Env a b c d) = fmap (\d' -> Env a b c d') (f d)
{-# ANN envSessionName ("HLint: ignore Avoid lambda" :: String) #-}

-- | Tear down a test session
tearDown :: Env -> IO ()
tearDown (Env _ dir mdir _) = do
  traverse_ removeDirectoryRecursive dir  -- remove session config dir if exists
  removeDirectoryRecursive mdir

-- | Set up a test session.
setUp :: GlobalEnv -> TmuxSession -> IO Env
setUp gEnv sessionName = do
  maildir <- setUpTempMaildir

  let
    -- For now, we never need to override the global config dir.
    -- But in the future, if we have tests for which we want to use
    -- a custom config, create the dir and set to 'Just dir'
    sessionConfDir = Nothing
    env = Env gEnv sessionConfDir maildir sessionName

  -- a) Make the regex less color code dependent by setting the TERM to 'ansi'.
  -- This can happen if different environments support more than 16 colours (e.g.
  -- background values > 37), while our CI environment only supports 16 colours.
  runReaderT (setEnvVarInSession "TERM" "ansi") env

  -- set the config dir
  runReaderT (view effectiveDir >>= setEnvVarInSession "PUREBRED_CONFIG_DIR") env

  pure env

precompileConfig :: FilePath -> IO ()
precompileConfig testdir = do
  env <- getEnvironment
  let systemEnv = ("PUREBRED_CONFIG_DIR", testdir) : env
      config = setEnv systemEnv $ proc "purebred" ["--version"]
  runProcess_ config

-- | Get the explicitly-specified source directory via SRCDIR
-- env var, or fall back to CWD.
getSourceDirectory :: IO FilePath
getSourceDirectory = lookupEnv "SRCDIR" >>= maybe getCurrentDirectory pure

setUpPurebredConfig :: FilePath -> IO ()
setUpPurebredConfig testdir = do
  c <- getSourceDirectory
  copyFile (c <> "/configs/purebred.hs") (testdir <> "/purebred.hs")

mkTempDir :: IO FilePath
mkTempDir = getCanonicalTemporaryDirectory >>= flip createTempDirectory "purebredtest"

-- | Set up a temporary Maildir containing the test database
-- The returned directory contains the 'Maildir' subdirectory.
setUpTempMaildir :: IO FilePath
setUpTempMaildir = do
  mdir <- mkTempDir
  cwd <- getSourceDirectory
  runProcess_ $ proc "cp" ["-r", cwd <> "/test/data/Maildir/", mdir]
  setUpNotmuchCfg mdir >>= setUpNotmuch
  pure mdir

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess_ $ proc "notmuch" ["--config=" <> notmuchcfg, "new" ]

-- | Write a minimal notmuch config pointing to the database.
setUpNotmuchCfg :: FilePath -> IO FilePath
setUpNotmuchCfg dir = do
  let cfgData = "[database]\npath=" <> dir <> "\n"
      cfgFile = dir <> "/notmuch-config"
  writeFile cfgFile cfgData $> cfgFile

purebredTmuxSession = withTmuxSession setUp tearDown

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: (MonadReader Env m, MonadIO m) => m ()
startApplication = do
  srcdir <- liftIO getSourceDirectory
  tmuxSendKeys LiteralKeys ("cd " <> srcdir <> "\r")
  testmdir <- view envMaildir
  tmuxSendKeys InterpretKeys ("purebred --database " <> testmdir <> "\r")
  void $ waitForCondition (Substring "Purebred: Item") defaultRetries defaultBackoff
