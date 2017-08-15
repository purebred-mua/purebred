module TestUserAcceptance where

import GHC.MVar (MVar)
import Control.Concurrent (newEmptyMVar, forkIO, putMVar, takeMVar, killThread, threadDelay)
import Control.Exception (bracket)

import System.Process (callProcess, readProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import System.Directory (getCurrentDirectory, removeFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import Network.Socket hiding (recv)
import Network.Socket
       (bind, socket, Family(..), SocketType(..), defaultProtocol, SockAddr(..))
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (pack, ByteString)

systemTests ::
  TestTree
systemTests = testGroup "result in the right state" [testMakesHardcopy]

testMakesHardcopy ::
  TestTree
testMakesHardcopy = goldenVsFile "does not crash" "test/data/test.golden" "/tmp/testoutput" smokeTest

smokeTest :: IO ()
smokeTest = do
  _ <- setUp =<< getCurrentDirectory
  callProcess "tmux" $ communicateSessionArgs ++ ["j", "j", "Enter"]
  out <- readProcess "tmux" hardcopyArgs []
  print out
  callProcess "tmux" $ savebufferArgs "/tmp/testoutput"
  teardown

waitReady :: MVar String -> IO ()
waitReady baton = do
    soc <- socket AF_UNIX Datagram defaultProtocol
    bind soc (SockAddrUnix "/tmp/purebred.socket")
    d <- recv soc 4096
    if d /= applicationReadySignal
        then error "application did not start up in time"
        else close soc >> removeFile "/tmp/purebred.socket" >> putMVar baton "ready"

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

setUp :: String -> IO (String)
setUp c = do
  baton <- newEmptyMVar
  bracket
    (forkIO $ waitReady baton)
    killThread
    (const $ callProcess "tmux" (tmuxSessionArgs c) >> callProcess "tmux" (communicateSessionArgs ++ ["C-t"]) >> print "Waiting for startup" >> takeMVar baton)

tmuxSessionArgs :: String -> [String]
tmuxSessionArgs c = ["new-session", "-P", "-F", "#{session_name}:#{command_name}-#{buffer_sample}", "-x", "95", "-y", "56", "-d", "-s", "purebredtest", "-n", "purebred", "-c", c, "purebred"]

communicateSessionArgs :: [String]
communicateSessionArgs = words "send-keys -t purebredtest"

hardcopyArgs :: [String]
hardcopyArgs = words "capture-pane -p -t purebredtest:purebred"

savebufferArgs :: FilePath -> [String]
savebufferArgs hardcopypath =
    ["save-buffer", "-b", "purebredcapture", hardcopypath]

teardown :: IO ()
teardown = do
  -- quit the application
  callProcess "tmux" $ communicateSessionArgs ++ (words "Esc Enter")
  -- XXX make sure it's dead
  callProcess "tmux" $ words "unlink-window -k -t purebredtest"
