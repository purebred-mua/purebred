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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isAscii, isAlphaNum, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Functor (($>))
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Data.Either (isRight)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath.Posix ((</>))
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Data.List (intercalate, isInfixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT)

import Control.Lens (Lens', view)
import System.Process.Typed
       (proc, runProcess_, withProcess_, readProcess_,
        waitExitCodeSTM, setStdout, getStdout, setStderr, useHandleOpen,
        byteStringOutput, setStdin, closed, setEnv, ProcessConfig)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive, removeFile, copyFile)
import Test.Tasty (TestTree, TestName, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

import Data.MIME (parse, message, mime, MIMEMessage)

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | A condition to check for in the output of the program
data Condition
  = Literal String
  | Regex String
  deriving (Show)

main :: IO ()
main = defaultMain $
  withResource pre post $ \_ ->
    testGroup "user acceptance tests" $ zipWith ($) tests [0..]
  where
    pre = let n = "keepalive" in setUpTmuxSession n $> n
    post = cleanUpTmuxSession
    tests =
      [ testUserViewsMailSuccessfully
      , testUserCanManipulateNMQuery
      , testUserCanSwitchBackToIndex
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
      ]

testFromAddressIsProperlyReset :: Int -> TestTree
testFromAddressIsProperlyReset = withTmuxSession "from address is reset to configured identity" $
  \step -> do
    testdir <- view (envDir . ask)
    setEnvVarInSession "GHC" "stack"
    setEnvVarInSession "GHC_ARGS" "\"$STACK_ARGS ghc --\""
    setEnvVarInSession "PUREBRED_CONFIG_DIR" testdir

    startApplication

    liftIO $ step "Start composing"
    sendKeys "m" (Literal "Joe Bloggs")

    liftIO $ step "abort editing"
    sendKeys "Escape" (Literal "tag:inbox")

    liftIO $ step "Start composing again"
    sendKeys "m" (Literal "Joe Bloggs")

    pure ()

testCanJumpToFirstListItem :: Int -> TestTree
testCanJumpToFirstListItem = withTmuxSession "updates read state for mail and thread" $
  \step -> do
    startApplication

    liftIO $ step "Jump to last mail"
    sendKeys "G" (Literal "3 of 3")

    liftIO $ step "Jump to first mail"
    sendKeys "1" (Literal "1 of 3")

    pure ()

testUpdatesReadState :: Int -> TestTree
testUpdatesReadState = withTmuxSession "updates read state for mail and thread" $
  \step -> do
    startApplication

    liftIO $ step "navigate to thread mails"
    sendKeys "G" (Literal "3 of 3")

    liftIO $ step "view unread mail in thread"
    sendKeys "Enter" (Literal "WIP Refactor")

    liftIO $ step "view next unread in thread"
    sendKeys "Down" (Literal "2 of 2")

    liftIO $ step "go back to thread list which is read"
    sendKeys "q q" (Regex (buildAnsiRegex [] ["37"] ["43"] <> " 08/Feb\\sRóman\\sJoost\\s+\\(2\\)"))

    liftIO $ step "set one mail to unread"
    sendKeys "Enter" (Literal "Beginning of large text")
    sendKeys "q t" (Regex (buildAnsiRegex ["1"] ["37"] [] <> "\\sWIP Refactor\\s" <> buildAnsiRegex ["0"] ["34"] ["40"]))

    liftIO $ step "returning to thread list shows thread unread"
    sendKeys "q" (Regex (buildAnsiRegex ["1"] ["37"] [] <> "\\sWIP Refactor\\s"))

    pure ()

testConfig :: Int -> TestTree
testConfig = withTmuxSession "test custom config" $
  \step -> do
    testdir <- view (envDir . ask)
    setEnvVarInSession "GHC" "stack"
    setEnvVarInSession "GHC_ARGS" "\"$STACK_ARGS ghc --\""
    setEnvVarInSession "PUREBRED_CONFIG_DIR" testdir

    startApplication

    liftIO $ step "archive thread"
    sendKeys "a" (Literal "archive")

    liftIO $ step "quit"
    sendKeys "q" (Literal "purebred")

    pure ()

testAddAttachments :: Int -> TestTree
testAddAttachments = withTmuxSession "use file browser to add attachments" $
  \step -> do
    testdir <- view (envDir . ask)
    setEnvVarInSession "GHC" "stack"
    setEnvVarInSession "GHC_ARGS" "\"$STACK_ARGS ghc --\""
    setEnvVarInSession "PUREBRED_CONFIG_DIR" testdir

    startApplication

    liftIO $ step "start composition"
    sendKeys "m" (Literal "From")

    liftIO $ step "enter from email"
    sendKeys "Enter" (Literal "To")

    liftIO $ step "enter to: email"
    sendKeys "user@to.test\r" (Literal "Subject")

    liftIO $ step "enter subject"
    sendKeys "test subject\r" (Literal "~")

    liftIO $ step "enter mail body"
    sendKeys "iThis is a test body" (Literal "body")

    liftIO $ step "exit insert mode in vim"
    sendKeys "Escape" (Literal "body")

    liftIO $ step "exit vim"
    sendKeys ": x\r" (Literal "Attachments")

    liftIO $ step "start file browser"
    cwd <- liftIO getCurrentDirectory
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] ["40"] <> cwd)

    liftIO $ step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - stack.yaml")

    liftIO $ step "add first selected file"
    sendKeys "Enter" (Literal $ cwd </> "stack.yaml")

    liftIO $ step "up to select mail body"
    sendKeys "Up" (Literal "Item 1 of 2")

    -- edit the mail body a few times to check if the code not mistakenly adds
    -- the same mail body as an attachment
    liftIO $ step "edit mail body text"
    sendKeys "e" (Literal "test body")

    liftIO $ step "append to mail body"
    sendKeys "i. foo" (Literal "foo")

    liftIO $ step "exit insert mode in vim"
    sendKeys "Escape" (Literal "foo")

    liftIO $ step "exit vim"
    sendKeys ": x\r" (Literal "Attachments")

    liftIO $ step "edit mail body text"
    sendKeys "e" (Literal "test body")

    liftIO $ step "append to mail body"
    sendKeys "i. foo" (Literal "foo")

    liftIO $ step "exit insert mode in vim"
    sendKeys "Escape" (Literal "foo")

    liftIO $ step "exit vim"
    sendKeys ": x\r" (Literal "Item 1 of 2")

    -- try removing attachments
    liftIO $ step "select the attachment"
    sendKeys "Down" (Literal "Item 2 of 2")

    liftIO $ step "remove the attachment"
    sendKeys "D" (Literal "Item 1 of 1")

    liftIO $ step "try to remove the last attachment"
    sendKeys "D" (Literal "You may not remove the only attachment")

    -- add the attachment again and send it
    liftIO $ step "start file browser"
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] ["40"] <> cwd)

    liftIO $ step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - stack.yaml")

    liftIO $ step "select the file"
    sendKeys "Space" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9745 - stack.yaml")

    liftIO $ step "move one item up"
    sendKeys "Up" (Regex $ buildAnsiRegex [] ["37"] ["43"] <> "\\s\9744 - screenshot.png")

    liftIO $ step "add selected files"
    out <- sendKeys "Enter" (Literal "Item 3 of 3")
    assertSubstrInOutput "screenshot.png" out

    liftIO $ step "send mail"
    sendKeys "y" (Literal "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = (chr . fromEnum <$> B.unpack contents)
    assertSubstrInOutput "attachment; filename" decoded
    assertSubstrInOutput "screenshot.png" decoded
    assertSubstrInOutput "stack.yaml" decoded
    assertSubstrInOutput "This is a test body" decoded

    pure ()

testManageTagsOnMails :: Int -> TestTree
testManageTagsOnMails = withTmuxSession "manage tags on mails" $
  \step -> do
    startApplication

    liftIO $ step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "focus command to show mail tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    liftIO $ step "enter new tag"
    _ <- sendLiteralKeys "+inbox +foo +bar"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "foo bar")
      >>= assertSubstrInOutput "This is a test mail"

    liftIO $ step "go back to list of mails"
    sendKeys "Escape" (Literal "Item 1 of 1")

    liftIO $ step "go back to list of threads"
    sendKeys "Escape" (Literal "Testmail")

    -- find newly tagged mail
    liftIO $ step "focus tag search"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

    liftIO $ step "enter tag to search `foo and bar`"
    _ <- sendLiteralKeys "tag:foo and tag:bar"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "tag:foo and tag:bar")

    liftIO $ step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "attempt to add a new tag"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    liftIO $ step "cancel tagging and expect old UI"
    -- instead of asserting the absence of the tagging editor, we assert the
    -- last visible "item" in the UI followed by whitespace. Give it an
    -- estimated upper range, since there could be some variable amount of
    -- whitespace.
    sendKeys "Escape" (Regex "This is a test mail for purebred\\s{4,8}$")

    pure ()

testManageTagsOnThreads :: Int -> TestTree
testManageTagsOnThreads = withTmuxSession "manage tags on threads" $
  \step -> do
    startApplication

    -- setup: tag the mails in the thread with two different threads and then
    -- tag the thread as a whole with a new tag. All mails should keep their
    -- distinct tags, while having received a new tag.
    liftIO $ step "navigate to thread"
    sendKeys "Down" (Literal "Item 2 of 3")
    sendKeys "Down" (Literal "Item 3 of 3")

    liftIO $ step "show thread mails"
    sendKeys "Enter" (Literal "ViewMail")

    liftIO $ step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    liftIO $ step "add new tag"
    _ <- sendLiteralKeys "+archive"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "archive")

    liftIO $ step "move to second mail"
    sendKeys "Down" (Literal "Item 2 of 2")

    liftIO $ step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    liftIO $ step "add new tag"
    _ <- sendLiteralKeys "+replied -inbox"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "replied")

    liftIO $ step "go back to list of mails"
    sendKeys "Escape" (Literal "Item 2 of 2")

    liftIO $ step "thread tags shows new tags"
    sendKeys "Escape" (Literal "archive inbox replied")

    liftIO $ step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    liftIO $ step "remove tag"
    -- "cheating" here a bit, since just invoking tmux with sending literally
    -- "-only" will fail due to tmux parsing it as an argument, but the mail is
    -- already tagged with "thread" so the additional adding won't do anything
    _ <- sendLiteralKeys "+thread"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "List of Threads")

    liftIO $ step "show thread mails"
    sendKeys "Enter" (Literal "ViewMail")

    liftIO $ step "navigate to second mail and assert shows old tag"
    sendKeys "Down" (Literal "Item 2 of 2")
    sendKeys "Escape" (Regex (buildAnsiRegex [] ["36"] []
                              <> "replied thread"
                              <> buildAnsiRegex [] ["37"] []
                              <> "\\sRe: WIP Refactor"))

    liftIO $ step "go back to list of threads"
    sendKeys "Escape" (Literal "List of Threads")

    liftIO $ step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    liftIO $ step "abort editing"
    sendKeys "Escape" (Literal "Query")

    pure ()

testHelp :: Int -> TestTree
testHelp = withTmuxSession "help view" $
  \step -> do
    startApplication

    liftIO $ step "shows Keybindings"
    sendKeys "?" (Literal "quit the application")

    sendKeys "Escape" (Literal "Purebred")
    pure ()

testErrorHandling :: Int -> TestTree
testErrorHandling = withTmuxSession "error handling" $
  \step -> do
    startApplication

    testmdir <- getTestMaildir
    liftIO $ removeFile (testmdir <> "/new/1502941827.R15455991756849358775.url")

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "shows error message"
    sendKeys "Enter" (Literal "FileReadError")
      >>= assertRegex "open(Binary)?File:.*does not exist"

    liftIO $ step "error is cleared with next registered keybinding"
    sendKeys "Up" (Literal "Purebred: Item 1 of 3")

    pure ()

testSetsMailToRead :: Int -> TestTree
testSetsMailToRead = withTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    liftIO $ step "first unread mail is opened"
    sendKeys "Escape" (Literal "List of Mails")
      >>= assertRegex (buildAnsiRegex [] ["37"] ["43"] <> ".*Testmail")

    liftIO $ step "toggle it back to unread (bold again)"
    sendKeys "t" (Regex (buildAnsiRegex ["1"] ["37"] ["43"] <> ".*Testmail"))
    pure ()

testCanToggleHeaders :: Int -> TestTree
testCanToggleHeaders = withTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")

    liftIO $ step "toggle to show all headers"
    sendKeys "h" (Regex "[Rr]eturn-[Pp]ath")

    liftIO $ step "toggle filtered headers"
    out <- sendKeys "h" (Literal "This is a test mail")
    assertRegex "Purebred.*\n.*[Ff]rom" out

testUserViewsMailSuccessfully :: Int -> TestTree
testUserViewsMailSuccessfully = withTmuxSession "user can view mail" $
  \step -> do
    startApplication
    liftIO $ step "shows tag"
    out <- capture
    assertSubstrInOutput "inbox" out
    assertSubstrInOutput "Testmail with whitespace in the subject" out

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail with whitespace in the subject")

    liftIO $ step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")

    liftIO $ step "go back to thread list"
    sendKeys "q q" (Literal "WIP Refactor")

    liftIO $ step "Move down to threaded mails"
    sendKeys "Down" (Literal "Purebred: Item 2 of 3")
    sendKeys "Down" (Literal "Purebred: Item 3 of 3")
    sendKeys "Enter" (Literal "Re: WIP Refactor")

    liftIO $ step "Scroll down"
    sendKeys "Enter" (Literal "Beginning of large text")
    sendKeys "Space" (Literal "Sed ut perspiciatis")

    liftIO $ step "go to next unread mail"
    sendKeys "j" (Literal "Re: WIP Refactor")

    liftIO $ step "Scroll down (again)"
    sendKeys "Space" (Literal "Sed ut perspiciatis")

    liftIO $ step "go to previous mail with reset scroll state"
    sendKeys "k" (Regex "Subject:\\s.*WIP Refactor")

    pure ()

testUserCanManipulateNMQuery :: Int -> TestTree
testUserCanManipulateNMQuery =
    withTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          liftIO $ step "focus command"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

          liftIO $ step "delete all input"
          sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

          liftIO $ step "search for non existing tags yielding no results"
          _ <- sendLiteralKeys "does not match anything"
          sendKeys "Enter" (Literal "No items")

          liftIO $ step "search for mail correctly tagged"
          sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "does"))
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

          liftIO $ step "enter new tag"
          _ <- sendLiteralKeys "tag:replied"

          liftIO $ step "apply"
          sendKeys "Enter" (Literal "Item 1 of 1")

          liftIO $ step "open thread"
          sendKeys "Enter" (Literal "This is Purebred")

          liftIO $ step "view currently selected mail"
          sendKeys "Enter" (Literal "HOLY PUREBRED")
          pure ()

testUserCanSwitchBackToIndex :: Int -> TestTree
testUserCanSwitchBackToIndex =
  withTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            liftIO $ step "start composition"
            sendKeys "m" (Literal "From")

            liftIO $ step "enter from email"
            sendKeys "testuser@foo.test\r" (Literal "To")

            liftIO $ step "enter to: email"
            sendKeys "user@to.test\r" (Literal "Subject")

            liftIO $ step "enter subject"
            sendKeys "test subject\r" (Literal "~")

            liftIO $ step "enter mail body"
            sendKeys "iThis is a test body" (Literal "body")

            liftIO $ step "exit insert mode in vim"
            sendKeys "Escape" (Literal "body")

            liftIO $ step "exit vim"
            sendKeys ": x\r" (Regex ("From: " <> buildAnsiRegex [] ["34"] [] <> "testuser@foo.test"))

            liftIO $ step "switch back to index"
            sendKeys "Tab" (Literal "Testmail")

            liftIO $ step "switch back to the compose editor"
            sendKeys "Tab" (Literal "test subject")

            liftIO $ step "cycle to next input field"
            sendKeys "C-n" (Regex (buildAnsiRegex [] ["33"] ["40"] <> "From:\\s+"
                                   <> buildAnsiRegex [] ["37"] [] <> "testuser@foo.test"))
            pure ()

testSendMail :: Int -> TestTree
testSendMail =
  withTmuxSession "sending mail successfully" $
        \step -> do
          testdir <- view (envDir . ask)
          setEnvVarInSession "GHC" "stack"
          setEnvVarInSession "GHC_ARGS" "\"$STACK_ARGS ghc --\""
          setEnvVarInSession "PUREBRED_CONFIG_DIR" testdir

          startApplication

          liftIO $ step "start composition"
          sendKeys "m" (Literal "From")

          liftIO $ step "append an additional from email"
          sendKeys ", testuser@foo.test\r" (Literal "To")

          liftIO $ step "enter to: email"
          sendKeys "user@to.test\r" (Literal "Subject")

          liftIO $ step "enter subject"
          let subj = ("test subject from directory " <> testdir)
          sendKeys (subj <> "\r") (Literal "~")

          liftIO $ step "enter mail body"
          sendKeys "iThis is a test body" (Literal "body")

          liftIO $ step "exit insert mode in vim"
          sendKeys "Escape" (Literal "body")

          liftIO $ step "exit vim"
          sendKeys ": x\r" (Literal "text/plain")
            >>= assertRegex ("From: "
                             <> buildAnsiRegex [] ["34"] []
                             <> "\"Joe Bloggs\" <joe@foo.test>, testuser@foo.test")

          liftIO $ step "user can re-edit body"
          sendKeys "e" (Literal "This is a test body")

          liftIO $ step "Writes more text"
          sendKeys "i. More text" (Literal "text")

          liftIO $ step "exit insert mode in vim"
          sendKeys "Escape" (Literal "body")

          liftIO $ step "exit vim"
          sendKeys ": x\r" (Regex ("text/plain\\s" <> buildAnsiRegex [] ["34"] ["40"] <> "\\s+"))

          liftIO $ step "send mail and go back to threads"
          sendKeys "y" (Regex ("Query:\\s" <> buildAnsiRegex [] ["34"] [] <> "tag:inbox"))

          liftIO $ step "parse mail with purebred-email"
          assertMailSuccessfullyParsed (testdir </> "sentMail")

          pure ()

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

data Env = Env
  { _envDir :: FilePath
  , _envMaildir :: FilePath
  , _envSessionName :: String
  }

envDir :: Lens' Env FilePath
envDir f (Env a b c) = fmap (\a' -> Env a' b c) (f a)

envMaildir :: Lens' Env FilePath
envMaildir f (Env a b c) = fmap (\b' -> Env a b' c) (f b)

envSessionName :: Lens' Env String
envSessionName f (Env a b c) = fmap (\c' -> Env a b c') (f c)
{-# ANN envSessionName ("HLint: ignore Avoid lambda" :: String) #-}

-- | Tear down a test session
tearDown :: Env -> IO ()
tearDown (Env testdir _ sessionName) = do
  removeDirectoryRecursive testdir
  cleanUpTmuxSession sessionName

-- | Set up a test session.
setUp :: Int -> String -> IO Env
setUp i desc = do
  let
    sessionName = intercalate "-" (sessionNamePrefix : show i : descWords)
    descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
  setUpTmuxSession sessionName
  prepareEnvironment sessionName
  (testdir, maildir) <- setUpTempMaildir
  setUpPurebredConfig testdir
  precompileConfig testdir
  pure $ Env testdir maildir sessionName

precompileConfig :: FilePath -> IO ()
precompileConfig testdir = do
  env <- getEnvironment
  let systemEnv = ("PUREBRED_CONFIG_DIR", testdir) : env
      config = setEnv systemEnv $ proc "stack" ["exec", "purebred", "--", "--version"]
  void $ runProcess_ config

setUpPurebredConfig :: FilePath -> IO ()
setUpPurebredConfig testdir = do
  c <- getCurrentDirectory
  copyFile (c <> "/configs/purebred.hs") (testdir <> "/purebred.hs")

setUpTempMaildir :: IO (String, String)
setUpTempMaildir = do
  systmp <- getCanonicalTemporaryDirectory
  testdir <- createTempDirectory systmp sessionNamePrefix
  mdir <- setUpMaildir testdir
  setUpNotmuchCfg testdir mdir >>= setUpNotmuch >> pure (testdir, mdir)

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess_ $ proc "notmuch" ["--config=" <> notmuchcfg, "new" ]

-- | write a notmuch config
-- Note: currently writes a minimal config pointing to our database
setUpNotmuchCfg :: FilePath -> FilePath -> IO FilePath
setUpNotmuchCfg testdir testmdir = do
  let (Right ini) = parseIni (T.pack "[database]\npath=" <> T.pack testmdir)
  let nmcfg = testdir <> "/notmuch-config"
  writeIniFileWith (WriteIniSettings EqualsKeySeparator) nmcfg ini
  pure nmcfg

-- | setup a temporary Maildir for notmuch and the test session
setUpMaildir :: FilePath -> IO FilePath
setUpMaildir testdir = do
  let testmdir = testdir <> "/Maildir/"
  c <- getCurrentDirectory
  let maildir = c <> "/test/data/Maildir/"
  runProcess_ $ proc "cp" ["-r", maildir, testmdir]
  pure testmdir

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
  -> ((String -> IO ()) -> ReaderT Env IO ())
  -> Int  -- ^ session sequence number (will be appended to session name)
  -> TestTree
withTmuxSession tcname testfx i =
  withResource (setUp i tcname) tearDown $
      \env -> testCaseSteps tcname $ \stepfx -> env >>= runReaderT (testfx stepfx)

-- | Send keys into the program and wait for the condition to be
-- met, failing the test if the condition is not met after some
-- time.
sendKeys :: String -> Condition -> ReaderT Env IO String
sendKeys keys expect = do
    sessionName <- getSessionName
    runProcess_ $ proc "tmux" $ communicateSessionArgs sessionName keys False
    waitForCondition expect defaultCountdown

sendLiteralKeys :: String -> ReaderT Env IO String
sendLiteralKeys keys = do
    sessionName <- getSessionName
    runProcess_ $ proc "tmux" $ communicateSessionArgs sessionName keys True
    waitForString keys defaultCountdown

capture :: ReaderT Env IO String
capture = do
  sessionname <- getSessionName
  liftIO $ readProcessWithErrorOutput_ $ proc "tmux" ["capture-pane", "-e", "-p", "-t", sessionname]

getSessionName :: (Monad m) => ReaderT Env m String
getSessionName = view (envSessionName . ask)

getTestMaildir :: (Monad m) => ReaderT Env m FilePath
getTestMaildir = view (envMaildir . ask)

holdOffTime :: Int
holdOffTime = 10 ^ (6 :: Int)

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | wait for the application to render a new interface which we determine with
--   a given condition. We check up to @n@ times, waiting a short duration
--   between each check, and failing if the tries exhaust with the condition
--   not met.
waitForCondition :: Condition -> Int -> ReaderT Env IO String
waitForCondition cond n = do
  out <- capture >>= checkPane
  liftIO $ assertBool
    ( "Wait time exceeded. Condition not met: '" <> show cond
      <> "' last screen shot:\n\n " <> out <> "\n\n" <> " raw: " <> show out )
    (checkCondition cond out)
  pure out
  where
    checkPane :: String -> ReaderT Env IO String
    checkPane out
      | checkCondition cond out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay holdOffTime
          waitForCondition cond (n - 1)

checkCondition :: Condition -> String -> Bool
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Convenience version of 'waitForCondition' that checks for a
-- literal string.
--
waitForString :: String -> Int -> ReaderT Env IO String
waitForString = waitForCondition . Literal

defaultCountdown :: Int
defaultCountdown = 5

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: ReaderT Env IO ()
startApplication = do
  testmdir <- getTestMaildir
  sessionName <- getSessionName
  -- add $STACK_ARGS so that we use the same resolver as was used for the build
  let cmdline = "stack $STACK_ARGS exec purebred -- --database " <> testmdir <> "\r"
  runProcess_ $ proc "tmux" $ communicateSessionArgs sessionName cmdline False
  void $ waitForString "Purebred: Item" defaultCountdown


-- | Prepare the environment
-- Here we're setting up the environment to run in a more predictable fasion.
--
-- a) Make the regex less color code dependent by setting the TERM to 'ansi'.
-- This can happen if different environments support more than 16 colours (e.g.
-- background values > 37), while our CI environment only supports 16 colours.
prepareEnvironment :: String -> IO ()
prepareEnvironment sessionName =
  runProcess_ $ proc "tmux" $
    communicateSessionArgs sessionName "export TERM=ansi\r" False

-- | Sets a shell environment variable
-- Note: The tmux program provides a command to set environment variables for
-- running sessions, yet they seem to be not inherited by the shell.
setEnvVarInSession :: String -> String -> ReaderT Env IO ()
setEnvVarInSession name value = do
  void $ sendLiteralKeys ("export " <> name <> "=" <> value)
  void $ sendKeys "Enter" (Literal name)

communicateSessionArgs
  :: String -- ^ session name
  -> String -- ^ keys
  -> Bool   -- ^ send the keys literally
  -> [String]
communicateSessionArgs sessionName keys asLiteral =
  ["send-keys", "-t", sessionName] <> ["-l" | asLiteral] <> [keys]


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

-- | Interleaves stderr with stdout
-- Does not accept STDIN arguments, since we do not need to communicate with the
-- process
readProcessWithErrorOutput_ :: ProcessConfig stdinClosed stdout stderr -> IO String
readProcessWithErrorOutput_ pc = do
  (_, out) <- withProcess_ config $ \p -> atomically $ (,) <$> waitExitCodeSTM p <*> getStdout p
  pure $ T.unpack $ decodeLenient $ LB.toStrict out
  where
    config = setStdout byteStringOutput
             $ setStderr (useHandleOpen stdout)
             $ setStdin closed pc
    decodeLenient = T.decodeUtf8With T.lenientDecode
