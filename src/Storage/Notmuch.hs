module Storage.Notmuch where

import Storage.Mail

import Notmuch
import Notmuch.Search

import Data.Foldable (toList)
import qualified Data.Vector as Vec
import Data.Maybe (fromMaybe)


getMessages :: String -> String -> IO (Vec.Vector Mail)
getMessages dbfp searchterms = do
  db' <- databaseOpen dbfp
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm searchterms)
        msgs <- messages q
        hdrs <- mapM messageToMail msgs
        return $ Vec.fromList $ toList hdrs

-- | TODO: use lenses to construct the mail
--
messageToMail :: Message -> IO Mail
messageToMail m = do
  s <- messageHeader "Subject" m
  t <- messageHeader "To" m
  f <- messageHeader "From" m
  pure $ Mail (fromMaybe "" s) (fromMaybe "" t) (fromMaybe "" f)
