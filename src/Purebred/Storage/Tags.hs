-- This file is part of purebred
-- Copyright (C) 2017-2021 Fraser Tweedale and Róman Joost
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

-- | This module provides functions to parse tagging operations. These
-- operations are used for annotating mails or threads with
-- labels. Notmuch does not support the labelling of threads as such
-- even though Purebred exposes the functionality. Labeling threads
-- just means labelling all mails in a thread.
--
module Purebred.Storage.Tags
  (
  -- * Constructing tags
    parseTag

  -- * Tag operations
  , TagOp(..)
  , parseTagOps

  -- * Managing an item's tags
  , ManageTags(..)
  , hasTag
  , tagItem
  , addTags
  , removeTags
  ) where

import Control.Applicative ((<|>), optional)
import Data.Attoparsec.ByteString.Char8
  ( Parser, parseOnly, isSpace, char, sepBy, takeWhile1 )
import qualified Data.ByteString as B
import Data.Functor (($>))
import Data.List (union)
import Control.Lens (Lens', over, set, view, _Left)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Notmuch (mkTag)

import Purebred.Types
import Purebred.Types.Parser.ByteString (niceEndOfInput, skipSpaces)
import Purebred.UI.Notifications (makeWarning)

-- | Tag operations
data TagOp = RemoveTag Tag | AddTag Tag | ResetTags
  deriving (Eq, Show)

tagOp :: Parser TagOp
tagOp =
  (char '+' *> (AddTag <$> takeTag))
  <|> (char '-' *> (RemoveTag <$> takeTag))
  where
  takeTag = takeWhile1 (not . isSpace) >>= parseTag

resetOp :: Parser TagOp
resetOp = char '=' $> ResetTags

allTagOps :: Parser [TagOp]
allTagOps = tagOp `sepBy` skipSpaces

tagOpsWithReset :: Parser [TagOp]
tagOpsWithReset = do
  r <- resetOp
  _ <- optional skipSpaces -- no space needed after '='
  ops <- allTagOps
  pure $ r : ops

parseTagOps :: T.Text -> Either UserMessage [TagOp]
parseTagOps = over _Left (makeWarning StatusBar . T.pack) . parseOnly p . T.encodeUtf8
  where
  p =
    (tagOpsWithReset <|> allTagOps)
    <* optional skipSpaces  -- skip any trailing whitespace
    <* niceEndOfInput       -- assert EOF

parseTag :: B.ByteString -> Parser Tag
parseTag s = maybe
  (fail $ "not a valid tag: " <> show s)
  pure
  (mkTag s)

-- | Tag either a 'NotmuchMail' or a 'NotmuchThread'
--
-- Tag operations are applied left to right.
--
tagItem :: ManageTags a => [TagOp] -> a -> a
tagItem ops mail = foldl (flip applyTagOp) mail ops

applyTagOp :: (ManageTags a) => TagOp -> a -> a
applyTagOp (AddTag t) = addTags [t]
applyTagOp (RemoveTag t) = removeTags [t]
applyTagOp ResetTags = setTags []

class ManageTags a  where
    tags :: Lens' a [Tag]

setTags :: (ManageTags a) => [Tag] -> a -> a
setTags = set tags

addTags :: (ManageTags a) => [Tag] -> a -> a
addTags tgs = over tags (`union` tgs)

removeTags :: (ManageTags a) => [Tag] -> a -> a
removeTags tgs = over tags (filter (`notElem` tgs))

hasTag :: (ManageTags a) => Tag -> a -> Bool
hasTag t x = t `elem` view tags x

instance ManageTags NotmuchMail where
  tags = mailTags

instance ManageTags NotmuchThread where
  tags = thTags
