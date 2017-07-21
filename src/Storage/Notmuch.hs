module Storage.Notmuch where

import Storage.Mail

import Notmuch
import Notmuch.Search

import Data.Foldable (toList)
import qualified Data.Vector as Vec


getMessages :: String -> IO (Vec.Vector Mail)
getMessages dbfp = do
  db' <- databaseOpen dbfp
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm "tag:inbox")
        msgs <- messages q
        hdrs <- mapM messageToMail msgs
        return $ Vec.fromList $ toList hdrs

messageToMail :: Message -> IO Mail
messageToMail m = Mail <$> messageHeader "Subject" m <*> pure "Test" <*> pure "From"
