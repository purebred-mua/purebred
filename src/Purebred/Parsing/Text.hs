-- This file is part of purebred
-- Copyright (C) 2017-2019 Fraser Tweedale and Róman Joost
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
{-# LANGUAGE OverloadedStrings #-}
module Purebred.Parsing.Text
  ( parseMailbody
  , niceEndOfInput
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Internal.Types as AT
import Data.Attoparsec.Text
import Text.Wrap (defaultWrapSettings, wrapTextToLines)
import qualified Data.Text as T
import Prelude hiding (Word)
import Control.Lens

import Purebred.Types

niceEndOfInput :: Parser ()
niceEndOfInput = endOfInput <|> p
  where
  p = do
    c <- peekChar'
    off <- offset
    fail $ "unexpected " <> show c <> " at offset " <> show off

-- | Get the current position of the parser
offset :: AT.Parser i Int
offset = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)

parseMailbody ::
     Int -- ^ text width
  -> Source
  -> T.Text
  -> MailBody
parseMailbody tw s =
  either
    (\e -> MailBody mempty [Paragraph [Line [] 0 (T.pack e)]])
    (MailBody s . setLineNumbers) . parseOnly (paragraphs tw <* niceEndOfInput)

endOfParagraph :: Parser ()
endOfParagraph = endOfLine *> endOfLine

paragraph :: Int -> Parser Paragraph
paragraph tw = Paragraph . makeLines tw . T.pack <$> manyTill anyChar endOfParagraph

paragraphs :: Int -> Parser [Paragraph]
paragraphs tw = do
  paras <- many' (paragraph tw)
  rest <- takeText
  pure $ paras <> [Paragraph $ makeLines tw rest]

makeLines :: Int -> T.Text -> [Line]
makeLines tw = fmap (Line [] 0) . wrapTextToLines defaultWrapSettings tw

setLineNumbers :: [Paragraph] -> [Paragraph]
setLineNumbers = iover (indexing (traversed . pLine)) (set lNumber)
