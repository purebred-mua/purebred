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
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isAscii, isAlphaNum, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Semigroup ((<>))
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath.Posix ((</>))
import Control.Monad (filterM, void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isInfixOf, sort)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT, ReaderT)

import Control.Lens (Getter, Lens', preview, to, view, _init, _last)
import System.Directory
  ( copyFile, getCurrentDirectory, listDirectory, removeDirectoryRecursive
  , removeFile
  )
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Process.Typed
       (proc, runProcess_, readProcess_, readProcessInterleaved_,
        setEnv, ProcessConfig)
import Test.Tasty (TestTree, TestName, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

import Data.MIME (parse, message, mime, MIMEMessage)

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | A condition to check for in the output of the program
data Condition
  = Unconditional
  | Literal String
  | Regex String
  deriving (Show)

type TestCase = IO GlobalEnv -> Int -> TestTree

main :: IO ()
main = defaultMain $
  withResource pre post $ \env ->
    testGroup "user acceptance tests" $ zipWith ($ env) tests [0..]
  where
    -- Create the tmux keepalive session and a config dir, with
    -- precompiled custom binary, that all test cases can use
    keepaliveSessionName = "keepalive"

    pre = do
      setUpTmuxSession keepaliveSessionName
      dir <- mkTempDir
      setUpPurebredConfig dir
      precompileConfig dir
      pure (GlobalEnv dir)

    -- Remove the shared config dir and kill the keepalive session
    post (GlobalEnv dir) = do
      cleanUpTmuxSession keepaliveSessionName
      removeDirectoryRecursive dir

    tests =
      [ testUserViewsMailSuccessfully
      , testUserCanManipulateNMQuery
      , testUserCanSwitchBackToIndex
      , testUserCanAbortMailComposition
      , testSendMail
      , testCanToggleHeaders
      , testSetsMailToRead
      , testErrorHandling
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
      ]

testEditingMailHeaders :: TestCase
testEditingMailHeaders = withTmuxSession "user can edit mail headers" $
  \step -> do
    startApplication

    step "start composition"
    sendKeys "m" (Literal "From")

    step "accept default"
    sendKeys "Enter" (Literal "To")

    step "enter to: email"
    sendKeys "user@to.test\r" (Literal "Subject")

    step "leave default"
    sendKeys "Enter" (Literal "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Literal "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Literal "body")

    step "exit vim"
    sendKeys ": x\r" (Literal "text/plain")
      >>= assertSubstrInOutput "From: \"Joe Bloggs\" <joe@foo.test>"

    step "user can change from header"
    sendKeys "f" (Regex $ "From: " <> buildAnsiRegex [] ["37"] [] <> "\"Joe Bloggs\" <joe@foo.test>")

    step "append an email"
    sendKeys ", testuser@foo.test\r" (Literal $ "From: "
                                      <> "\"Joe Bloggs\" <joe@foo.test>, testuser@foo.test")

    step "user can change to header"
    sendKeys "t" (Regex $ "To: " <> buildAnsiRegex [] ["37"] [] <> "user@to.test")

    step "append an additional from email"
    sendKeys ", testuser@foo.test\r" (Literal "To: user@to.test, testuser@foo.test")

    step "change subject"
    sendKeys "s" (Regex $ "Subject: " <> buildAnsiRegex [] ["37"] [] <> "")

    step "enter subject"
    sendKeys "foo subject\r" (Literal "Subject: foo subject")

testPipeEntitiesSuccessfully :: TestCase
testPipeEntitiesSuccessfully = withTmuxSession "pipe entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Literal "text/plain")

    step "pipe to"
    sendKeys "|" (Literal "Pipe to")

    step "use less"
    _ <- sendLiteralKeys "less"
    sendKeys "Enter" (Regex ("This is a test mail for purebred"
                             <> buildAnsiRegex [] ["37"] ["40"]
                             <> "\\s+"
                             <> buildAnsiRegex ["7"] ["39"] ["49"]
                             <> "\\(END\\)"))

testOpenEntitiesSuccessfully :: TestCase
testOpenEntitiesSuccessfully = withTmuxSession "open entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Literal "text/plain")

    step "open one entity"
    sendKeys "o" (Literal "Open With")
    _ <- sendLiteralKeys "less"

    sendKeys "Enter" (Regex ("This is a test mail for purebred"
                            <> buildAnsiRegex [] ["37"] ["40"]
                            <> "\\s+"
                            <> buildAnsiRegex ["7"] ["39"] ["49"]
                            <> ".*purebred.*END"))

testOpenCommandDoesNotKillPurebred :: TestCase
testOpenCommandDoesNotKillPurebred = withTmuxSession "open attachment does not kill purebred" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Literal "text/plain")

    step "open with"
    sendKeys "o" (Literal "Open With")

    step "Open with bogus command"
    _ <- sendLiteralKeys "asdfasdfasdf"
    sendKeys "Enter" (Literal "ProcessError")

testShowsMailEntities :: TestCase
testShowsMailEntities = withTmuxSession "shows mail entities successfully" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Literal "text/plain")

    step "select the second entity"
    sendKeys "j" (Literal "text/html")

    step "close the list of entities"
    out <- sendKeys "q" (Literal "This is a test mail for purebred")

    -- poor mans (?!text)
    assertRegex "[^t][^e][^x][^t]" out

testUserCanMoveBetweenThreads :: TestCase
testUserCanMoveBetweenThreads = withTmuxSession "user can navigate between threads" $
  \step -> do
    startApplication
    -- assert that the first mail is really the one we're later navigating back
    -- to
    out <- capture
    assertRegex (buildAnsiRegex ["1"] ["37"] ["43"] <> "\\sAug'17.*Testmail with whitespace") out

    step "View Mail"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "Navigate down the threads list"
    sendKeys "J" (Literal "HOLY PUREBRED")

    step "Navigate up the threads list"
    sendKeys "K" (Literal "This is a test mail for purebred")

testRepliesToMailSuccessfully :: TestCase
testRepliesToMailSuccessfully = withTmuxSession "replies to mail successfully" $
  \step -> do
    let subject = "Testmail with whitespace in the subject"
    testdir <- view effectiveDir
    startApplication

    step "pick first mail"
    out <- sendKeys "Enter" (Literal "This is a test mail for purebred")

    assertSubstrInOutput "From: <roman@host.example>" out
    assertSubstrInOutput "To: <frase@host.example>" out
    assertSubstrInOutput ("Subject: " <> subject) out

    step "start replying"
    sendKeys "r" (Literal "> This is a test mail for purebred")

    step "exit vim"
    out' <- sendKeys ": x\r" (Literal "Attachments")

    assertSubstrInOutput "From: <frase@host.example>" out'
    assertSubstrInOutput "To: <roman@host.example>" out'
    assertSubstrInOutput ("Subject: Re: " <> subject) out'

    step "send mail"
    sendKeys "y" (Literal "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstrInOutput ("Subject: Re: " <> subject) decoded
    assertSubstrInOutput "From: frase@host.example" decoded
    assertSubstrInOutput "To: roman@host.example" decoded
    assertSubstrInOutput "> This is a test mail for purebred" decoded

testFromAddressIsProperlyReset :: TestCase
testFromAddressIsProperlyReset = withTmuxSession "from address is reset to configured identity" $
  \step -> do
    startApplication

    step "Start composing"
    sendKeys "m" (Literal "Joe Bloggs")

    step "abort editing"
    sendKeys "Escape" (Literal "tag:inbox")

    step "Start composing again"
    sendKeys "m" (Literal "Joe Bloggs")

testCanJumpToFirstListItem :: TestCase
testCanJumpToFirstListItem = withTmuxSession "can jump to first and last mail" $
  \step -> do
    startApplication

    step "Jump to last mail"
    sendKeys "G" (Literal "3 of 3")

    step "Jump to first mail"
    sendKeys "1" (Literal "1 of 3")

testUpdatesReadState :: TestCase
testUpdatesReadState = withTmuxSession "updates read state for mail and thread" $
  \step -> do
    startApplication

    step "navigate to thread mails"
    sendKeys "G" (Literal "3 of 3")

    step "view unread mail in thread"
    sendKeys "Enter" (Literal "WIP Refactor")

    step "view next unread in thread"
    sendKeys "Down" (Literal "2 of 2")

    step "go back to thread list which is read"
    sendKeys "q q" (Regex (buildAnsiRegex [] ["37"] ["43"] <> " Feb'17\\sRóman\\sJoost\\s+\\(2\\)"))

    step "set one mail to unread"
    sendKeys "Enter" (Literal "Beginning of large text")
    sendKeys "q t" (Regex (buildAnsiRegex ["1"] ["37"] []
                           <> "\\sRe: WIP Refactor\\s+"
                           <> buildAnsiRegex ["0"] ["34"] ["40"]))

    step "returning to thread list shows thread unread"
    sendKeys "q" (Regex (buildAnsiRegex ["1"] ["37"] [] <> "\\sWIP Refactor\\s"))

testConfig :: TestCase
testConfig = withTmuxSession "test custom config" $
  \step -> do
    -- Set a short command prompt, to a value otherwise unlikely to
    -- appear, so that we can easily check for program termination.
    let unlikelyString = "unlikely"
    sendKeys ("PS1=" <> unlikelyString <> "$ \r") (Literal unlikelyString)
    startApplication

    step "archive thread"
    sendKeys "a" (Literal "archive")

    step "quit"
    sendKeys "q" Unconditional

    -- Wait a bit so that purebred, which may not yet have
    -- terminated, does not eat the upcoming keystroke(s)
    liftIO $ threadDelay 200000  -- 0.2 seconds

    -- Press Enter again to deal with case where cursor is not at
    -- column 0, which could cause target string to be split.
    sendKeys "Enter" (Literal unlikelyString)

testAddAttachments :: TestCase
testAddAttachments = withTmuxSession "use file browser to add attachments" $
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
    sendKeys "m" (Literal "From")

    step "enter from email"
    sendKeys "Enter" (Literal "To")

    step "enter to: email"
    sendKeys "user@to.test\r" (Literal "Subject")

    step "enter subject"
    sendKeys "test subject\r" (Literal "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Literal "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Literal "body")

    step "exit vim"
    sendKeys ": x\r" (Literal "Attachments")

    step "start file browser"
    cwd <- liftIO getCurrentDirectory
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] ["40"] <> cwd)

    step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - " <> lastFile)

    step "add first selected file"
    sendKeys "Enter" (Literal lastFile)

    step "up to select mail body"
    sendKeys "Up" (Literal "Item 1 of 2")

    -- edit the mail body a few times to check if the code not mistakenly adds
    -- the same mail body as an attachment
    step "edit mail body text"
    sendKeys "e" (Literal "test body")

    step "append to mail body"
    sendKeys "i. foo" (Literal "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Literal "foo")

    step "exit vim"
    sendKeys ": x\r" (Literal "Attachments")

    step "edit mail body text"
    sendKeys "e" (Literal "test body")

    step "append to mail body"
    sendKeys "i. foo" (Literal "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Literal "foo")

    step "exit vim"
    sendKeys ": x\r" (Literal "Item 1 of 2")

    -- try removing attachments
    step "select the attachment"
    sendKeys "Down" (Literal "Item 2 of 2")

    step "remove the attachment"
    sendKeys "D" (Literal "Item 1 of 1")

    step "try to remove the last attachment"
    sendKeys "D" (Literal "You may not remove the only attachment")

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
    out <- sendKeys "Enter" (Literal "Item 3 of 3")
    assertSubstrInOutput secondLastFile out

    step "send mail"
    sendKeys "y" (Literal "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstrInOutput "attachment; filename" decoded
    assertSubstrInOutput secondLastFile decoded
    assertSubstrInOutput lastFile decoded
    assertSubstrInOutput "This is a test body" decoded

testManageTagsOnMails :: TestCase
testManageTagsOnMails = withTmuxSession "manage tags on mails" $
  \step -> do
    startApplication

    step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    step "focus command to show mail tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter new tag"
    _ <- sendLiteralKeys "+inbox +foo +bar"

    step "apply"
    sendKeys "Enter" (Regex ("foo"
                             <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] []
                             <> "bar"))
      >>= assertSubstrInOutput "This is a test mail"

    step "go back to list of mails"
    sendKeys "Escape" (Literal "List of Mails")

    step "go back to list of threads"
    sendKeys "Escape" (Literal "List of Threads")

    -- find newly tagged mail
    step "focus tag search"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter tag to search `foo and bar`"
    _ <- sendLiteralKeys "tag:foo and tag:bar"

    step "apply"
    sendKeys "Enter" (Literal "tag:foo and tag:bar")

    step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    step "attempt to add a new tag"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "cancel tagging and expect old UI"
    -- instead of asserting the absence of the tagging editor, we assert the
    -- last visible "item" in the UI followed by whitespace.
    sendKeys "Escape" (Regex "This is a test mail for purebred\\s+$")

testManageTagsOnThreads :: TestCase
testManageTagsOnThreads = withTmuxSession "manage tags on threads" $
  \step -> do
    startApplication

    -- setup: tag the mails in the thread with two different tags and then
    -- tag the thread as a whole with a new tag. All mails should keep their
    -- distinct tags, while having received a new tag.
    step "navigate to thread"
    sendKeys "Down" (Literal "Item 2 of 3")
    sendKeys "Down" (Literal "Item 3 of 3")

    step "show thread mails"
    sendKeys "Enter" (Literal "ViewMail")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    _ <- sendLiteralKeys "+archive"

    step "apply"
    sendKeys "Enter" (Literal "archive")

    step "move to second mail"
    sendKeys "Down" (Literal "Item 2 of 2")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    _ <- sendLiteralKeys "+replied -inbox"

    step "apply"
    sendKeys "Enter" (Literal "replied")

    step "go back to list of mails"
    sendKeys "Escape" (Literal "Item 2 of 2")

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
    _ <- sendLiteralKeys "+thread"

    step "apply"
    sendKeys "Enter" (Regex ("archive"
                             <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "replied" <> buildAnsiRegex [] ["37"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "thread"))

    step "show thread mails"
    sendKeys "Enter" (Literal "ViewMail")

    step "second mail shows old tag"
    sendKeys "Escape" (Regex ("replied"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\s"
                              <> buildAnsiRegex [] ["36"] []
                              <> "thread"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\sRe: WIP Refactor"))

    step "go back to list of threads"
    sendKeys "Escape" (Literal "List of Threads")

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "abort editing"
    sendKeys "Escape" (Literal "Query")

testHelp :: TestCase
testHelp = withTmuxSession "help view" $
  \step -> do
    startApplication

    step "shows Keybindings"
    sendKeys "?" (Literal "quit the application")

    sendKeys "Escape" (Literal "Purebred")

testErrorHandling :: TestCase
testErrorHandling = withTmuxSession "error handling" $
  \step -> do
    startApplication

    testmdir <- view envMaildir
    liftIO $ removeFile (testmdir <> "/Maildir/new/1502941827.R15455991756849358775.url")

    step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    step "shows error message"
    sendKeys "Enter" (Literal "FileReadError")
      >>= assertRegex "open(Binary)?File:.*does not exist"

    step "error is cleared with next registered keybinding"
    sendKeys "Up" (Literal "Purebred: Item 1 of 3")

testSetsMailToRead :: TestCase
testSetsMailToRead = withTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    step "first unread mail is opened"
    sendKeys "Escape" (Literal "List of Mails")
      >>= assertRegex (buildAnsiRegex [] ["37"] ["43"] <> ".*Testmail")

    step "toggle it back to unread (bold again)"
    sendKeys "t" (Regex (buildAnsiRegex ["1"] ["37"] ["43"] <> ".*Testmail"))

testCanToggleHeaders :: TestCase
testCanToggleHeaders = withTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")

    step "toggle to show all headers"
    sendKeys "h" (Regex "[Rr]eturn-[Pp]ath")

    step "toggle filtered headers"
    out <- sendKeys "h" (Literal "This is a test mail")
    assertRegex "Purebred.*\n.*[Ff]rom" out

testUserViewsMailSuccessfully :: TestCase
testUserViewsMailSuccessfully = withTmuxSession "user can view mail" $
  \step -> do
    startApplication
    step "shows tag"
    out <- capture
    assertSubstrInOutput "inbox" out
    assertSubstrInOutput "Testmail with whitespace in the subject" out

    step "open thread"
    sendKeys "Enter" (Literal "Testmail with whitespace in the subject")

    step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")

    step "go back to thread list"
    sendKeys "q q" (Literal "WIP Refactor")

    step "Move down to threaded mails"
    sendKeys "Down" (Literal "Purebred: Item 2 of 3")
    sendKeys "Down" (Literal "Purebred: Item 3 of 3")
    sendKeys "Enter" (Literal "Re: WIP Refactor")

    step "Scroll down"
    sendKeys "Enter" (Literal "Beginning of large text")
    sendKeys "Space" (Literal "Sed ut perspiciatis")

    step "go to next unread mail"
    sendKeys "j" (Literal "Re: WIP Refactor")

    step "Scroll down (again)"
    sendKeys "Space" (Literal "Sed ut perspiciatis")

    step "go to previous mail with reset scroll state"
    sendKeys "k" (Regex "Subject:\\s.*WIP Refactor")

testUserCanManipulateNMQuery :: TestCase
testUserCanManipulateNMQuery =
    withTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          step "focus command"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

          step "delete all input"
          sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

          step "search for non existing tags yielding no results"
          _ <- sendLiteralKeys "does not match anything"
          sendKeys "Enter" (Literal "No items")

          step "search for mail correctly tagged"
          sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "does"))
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

          step "enter new tag"
          _ <- sendLiteralKeys "tag:replied"

          step "apply"
          sendKeys "Enter" (Literal "Item 1 of 1")

          step "open thread"
          sendKeys "Enter" (Literal "This is Purebred")

          step "view currently selected mail"
          sendKeys "Enter" (Literal "HOLY PUREBRED")

testUserCanSwitchBackToIndex :: TestCase
testUserCanSwitchBackToIndex =
  withTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            step "start composition"
            sendKeys "m" (Literal "From")

            step "enter from email"
            sendKeys "C-a" Unconditional
            sendKeys "C-k" Unconditional
            sendKeys "testuser@foo.test\r" (Literal "To")

            step "enter to: email"
            sendKeys "user@to.test\r" (Literal "Subject")

            step "enter subject"
            sendKeys "test subject\r" (Literal "~")

            step "enter mail body"
            sendKeys "iThis is a test body" (Literal "body")

            step "exit insert mode in vim"
            sendKeys "Escape" (Literal "body")

            step "exit vim"
            sendKeys ": x\r" (Regex "From: testuser@foo.test")

            step "switch back to index"
            sendKeys "Tab" (Literal "Testmail")

            step "switch back to the compose editor"
            sendKeys "Tab" (Literal "test subject")

testUserCanAbortMailComposition :: TestCase
testUserCanAbortMailComposition =
  withTmuxSession "user can abort composing mail" $
        \step -> do
            startApplication
            step "start composition"
            sendKeys "m" (Literal "From")

            step "enter from email"
            sendKeys "Enter" (Literal "To")

            step "enter to: email"
            sendKeys "user@to.test\r" (Literal "Subject")

            step "enter subject"
            sendKeys "test subject\r" (Literal "~")

            step "enter mail body"
            sendKeys "iThis is a test body" (Literal "body")

            step "exit insert mode in vim"
            sendKeys "Escape" (Literal "body")

            step "exit vim"
            sendKeys ": x\r" (Regex $ "From: "
                             <> "\"Joe Bloggs\" <joe@foo.test>")

            step "abort mail"
            sendKeys "q" (Literal "Testmail")

            step "start composition again"
            sendKeys "m" (Literal "From")
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


testSendMail :: TestCase
testSendMail =
  withTmuxSession "sending mail successfully" $
        \step -> do
          testdir <- view effectiveDir
          startApplication

          step "start composition"
          sendKeys "m" (Literal "From")

          step "append an additional from email"
          sendKeys ", testuser@foo.test\r" (Literal "To")

          step "enter to: email"
          sendKeys "user@to.test\r" (Literal "Subject")

          step "enter subject"
          let subj = "test subject from directory " <> testdir
          sendKeys (subj <> "\r") (Literal "~")

          step "enter mail body"
          sendKeys "iThis is a test body" (Literal "body")

          step "exit insert mode in vim"
          sendKeys "Escape" (Literal "body")

          step "exit vim"
          sendKeys ": x\r" (Literal "text/plain")
            >>= assertRegex ("From: "
                             <> "\"Joe Bloggs\" <joe@foo.test>, testuser@foo.test")

          step "user can re-edit body"
          sendKeys "e" (Literal "This is a test body")

          step "Writes more text"
          sendKeys "i. More text" (Literal "text")

          step "exit insert mode in vim"
          sendKeys "Escape" (Literal "body")

          step "exit vim"
          sendKeys ": x\r" (Regex ("text/plain\\s" <> buildAnsiRegex [] ["34"] ["40"] <> "\\s+"))

          step "send mail and go back to threads"
          sendKeys "y" (Regex ("Query:\\s" <> buildAnsiRegex [] ["34"] [] <> "tag:inbox"))

          step "parse mail with purebred-email"
          assertMailSuccessfullyParsed (testdir </> "sentMail")


parseMail :: B.ByteString -> Either String MIMEMessage
parseMail = parse (message mime)

assertMailSuccessfullyParsed :: String -> ReaderT a IO ()
assertMailSuccessfullyParsed fp = do
  contents <- liftIO $ B.readFile fp
  let result = parseMail contents
  liftIO $ assertBool "expected successful MIMEMessage" (isRight result)

assertSubstrInOutput :: String -> String -> ReaderT a IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT a IO ()
assertRegex regex out = liftIO $ assertBool
  (show regex <> " does not match out\n\n" <> out
    <> "\n\n raw:\n\n" <> show out)
  (out =~ regex)

sessionNamePrefix :: String
sessionNamePrefix = "purebredtest"

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

type TmuxSession = String

class HasTmuxSession a where
  tmuxSession :: Lens' a TmuxSession

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
tearDown (Env _ dir mdir sessionName) = do
  traverse_ removeDirectoryRecursive dir  -- remove session config dir if exists
  removeDirectoryRecursive mdir
  cleanUpTmuxSession sessionName

-- | Set up a test session.
setUp :: IO GlobalEnv -> Int -> String -> IO Env
setUp getGEnv i desc = do
  let
    sessionName = intercalate "-" (sessionNamePrefix : show i : descWords)
    descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
  setUpTmuxSession sessionName
  maildir <- setUpTempMaildir

  gEnv <- getGEnv

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
mkTempDir = getCanonicalTemporaryDirectory >>= flip createTempDirectory sessionNamePrefix

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

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
setUpTmuxSession :: String -> IO ()
setUpTmuxSession sessionname =
    catch
        (runProcess_ $ proc
             "tmux"
             [ "new-session"
             , "-x"
             , "80"
             , "-y"
             , "24"
             , "-d"
             , "-s"
             , sessionname
             , "-n"
             , "purebred"])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException during setUp: " <> err)
                 pure ())

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname =
    catch
        (runProcess_ $ proc "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException when killing session: " <> err)
                 pure ())


-- | Run all application steps in a session defined by session name.
withTmuxSession
  :: TestName
  -> ((String -> ReaderT Env IO ()) -> ReaderT Env IO a)
  -> IO GlobalEnv
  -> Int  -- ^ session sequence number (will be appended to session name)
  -> TestTree
withTmuxSession tcname testfx gEnv i =
  withResource (setUp gEnv i tcname) tearDown $
      \env -> testCaseSteps tcname $
        \step -> env >>= runReaderT (void $ testfx (liftIO . step))

-- | Send keys into the program and wait for the condition to be
-- met, failing the test if the condition is not met after some
-- time.
sendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Condition -> m String
sendKeys keys expect = do
    tmuxSendKeys InterpretKeys keys
    waitForCondition expect defaultCountdown initialBackoffMicroseconds

sendLiteralKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> m String
sendLiteralKeys keys = do
    tmuxSendKeys LiteralKeys keys
    waitForString keys defaultCountdown

capture :: (HasTmuxSession a, MonadReader a m, MonadIO m) => m String
capture = T.unpack . decodeLenient . LB.toStrict
  <$> (tmuxSessionProc "capture-pane"
    [ "-e"  -- include escape sequences
    , "-p"  -- send output to stdout
    , "-J"  -- join wrapped lines and preserve trailing whitespace
    ]
  >>= liftIO . readProcessInterleaved_)
  where
    decodeLenient = T.decodeUtf8With T.lenientDecode

initialBackoffMicroseconds :: Int
initialBackoffMicroseconds = 20 * 10 ^ (3 :: Int)

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | wait for the application to render a new interface which we determine with
--   a given condition. We wait a short duration and increase the wait time
--   exponentially until the count down reaches 0. We fail if until then the
--   condition is not met.
waitForCondition
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => Condition
  -> Int  -- ^ count down value
  -> Int  -- ^ milliseconds to back off
  -> m String
waitForCondition cond n backOff = do
  out <- capture >>= checkPane
  liftIO $ assertBool
    ( "Wait time exceeded. Condition not met: '" <> show cond
      <> "' last screen shot:\n\n " <> out <> "\n\n" <> " raw: " <> show out )
    (checkCondition cond out)
  pure out
  where
    checkPane out
      | checkCondition cond out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay backOff
          waitForCondition cond (n - 1) (backOff * 4)

checkCondition :: Condition -> String -> Bool
checkCondition Unconditional = const True
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Convenience version of 'waitForCondition' that checks for a
-- literal string.
--
waitForString
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Int -> m String
waitForString substr n = waitForCondition (Literal substr) n initialBackoffMicroseconds

defaultCountdown :: Int
defaultCountdown = 5

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: ReaderT Env IO ()
startApplication = do
  srcdir <- liftIO getSourceDirectory
  tmuxSendKeys LiteralKeys ("cd " <> srcdir <> "\r")
  testmdir <- view envMaildir
  tmuxSendKeys InterpretKeys ("purebred --database " <> testmdir <> "\r")
  void $ waitForString "Purebred: Item" defaultCountdown

-- | Sets a shell environment variable
-- Note: The tmux program provides a command to set environment variables for
-- running sessions, yet they seem to be not inherited by the shell.
setEnvVarInSession
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> String -> m ()
setEnvVarInSession name value = do
  void $ sendLiteralKeys ("export " <> name <> "=" <> value)
  void $ sendKeys "Enter" (Literal name)

-- | Whether to tell tmux to treat keys literally or interpret
-- sequences like "Enter" or "C-x".
--
data TmuxKeysMode = LiteralKeys | InterpretKeys
  deriving (Eq)

-- | Run a tmux command via 'runProcess_'.  The session name is read
-- from the 'MonadReader' environment
--
tmuxSendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => TmuxKeysMode -> String -> m ()
tmuxSendKeys mode keys = tmuxSendKeysProc mode keys >>= runProcess_

-- | Construct the 'ProcessConfig' for a tmux command.  The session
-- name is read from the 'MonadReader' environment.
--
tmuxSendKeysProc
  :: (HasTmuxSession a, MonadReader a m)
  => TmuxKeysMode -> String -> m (ProcessConfig () () ())
tmuxSendKeysProc mode keys = tmuxSessionProc "send-keys" (["-l" | mode == LiteralKeys] <> [keys])

-- | Create a 'ProcessConfig' for a tmux command, taking the session
-- name from the 'MonadReader' environment.
--
tmuxSessionProc
  :: (HasTmuxSession a, MonadReader a m)
  => String -> [String] -> m (ProcessConfig () () ())
tmuxSessionProc cmd args = do
  sessionName <- view tmuxSession
  pure $ proc "tmux" (cmd : "-t" : sessionName : args)



type AnsiAttrParam = String
type AnsiFGParam = String
type AnsiBGParam = String

-- | Generate a regex for an escape sequence setting the given
-- foreground and background parameters
--
-- tmux < 03d01eabb5c5227f56b6b44d04964c1328802628 (first released
-- in tmux-2.5) ran attributes, foreground colour and background
-- colour params separated by semicolons (foreground first).
--
-- After that commit, attributes, foreground colours and background
-- colours are written in separate escape sequences.  Therefore for
-- compatibility with different versions of tmux there are two
-- patterns to check.
--
buildAnsiRegex :: [AnsiAttrParam] -> [AnsiFGParam] -> [AnsiBGParam] -> String
buildAnsiRegex attrs fgs bgs =
  let
    withSemis = intercalate ";"
    wrap [] = ""
    wrap xs = "\ESC\\[" <> withSemis xs <> "m"
    tmux24 = wrap (attrs <> fgs <> bgs)
    tmux25 = wrap attrs <> wrap fgs <> wrap bgs
    choice "" "" = ""
    choice "" r = r
    choice l "" = l
    choice l r = "(" <> l <> "|" <> r <> ")"
  in
    choice tmux24 tmux25
