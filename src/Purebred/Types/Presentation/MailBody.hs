-- This file is part of purebred
-- Copyright (C) 2022  Fraser Tweedale
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

{-# LANGUAGE OverloadedStrings #-}

module Purebred.Types.Presentation.MailBody
  ( MailBody(..)
  , mkPresentation
  , parseMailbody
  ) where

import Control.Lens (ix, preview)
import Control.Monad (guard)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Brick
  ( Padding(..), Widget
  , (<=>), hBox, padBottom, txt, vBox, withAttr )
import Text.Wrap (defaultWrapSettings, wrapTextToLines)

import Purebred.Types.Presentation
import Purebred.UI.Attr
  ( currentTextMatchHighlightAttr
  , mailbodySourceAttr, textMatchHighlightAttr )

-- | A loose annotation what produced the rendered output of the
-- entity
--
type Source = T.Text

-- | Type representing a specific entity from an e-mail for display,
-- optionally with search matches to be highlighted.
--
data MailBody = MailBody Source [T.Text]
  deriving (Show, Eq)

-- | A match of a substring in the current line of text
--
data Match =
  Match Int -- ^ offset
        Int -- ^ length
        Int -- ^ line number
  deriving (Show, Eq)

-- | Construct an initial presentation with no substring search
-- applied.
mkPresentation :: MailBody -> BodyPresentation
mkPresentation = mkPresentation' Nothing

mkPresentation' :: Maybe [Match] -> MailBody -> BodyPresentation
mkPresentation' matches mb@(MailBody _ lns) =
  let
    matchAt nat = matches >>= \ms -> do
      guard (nat <= fromIntegral (maxBound :: Int))
      preview (ix (fromIntegral nat)) ms
    matchLine = maybe
      NoMatch
      -- +2 lines for "Showing output from: ..." front-matter
      (\(Match _ _ l) -> MatchAtLine (fromIntegral l + 2))
  in
    BodyPresentation
      { widget =
          \nat ->
            let
              match = matchAt nat
            in
              (matchLine match, renderMarkup match (fromMaybe [] matches) mb)

      , substringSearch =
          \term ->
            let
              newMatches = findMatchingWords term lns
              matchInfo =
                maybe NoSearch (MatchCount . fromIntegral . length) newMatches
            in
              (matchInfo, mkPresentation' newMatches mb)

      , toTextLines = lns
      }

parseMailbody :: Int {- ^ text width -} -> Source -> T.Text -> BodyPresentation
parseMailbody tw s = mkPresentation .
  MailBody s . wrapTextToLines defaultWrapSettings tw


-- | Find matching words in the body.  Matching is case sensitive.
--
findMatchingWords :: T.Text -> [T.Text] -> Maybe [Match]
findMatchingWords ""     _  = Nothing
findMatchingWords needle lns = Just $ foldMap go (zip [0..] lns)
  where
  go (i, s) =
    (\(h, _) -> Match (T.length h) (T.length needle) i)
    <$> T.breakOnAll needle s

-- | render the Mailbody AST to a list used for Markup in Brick
--
renderMarkup :: Maybe Match -> [Match] -> MailBody -> Widget a
renderMarkup cur matches (MailBody src lns) = source <=> vBox markups
  where
  source =
    withAttr mailbodySourceAttr $
    padBottom (Pad 1) $ txt ("Showing output from: " <> src)

  markups = markupLines matches (zip [0..] lns)

  markupLines :: [Match] -> [(Int, T.Text)] -> [Widget a]
  markupLines _  []     = []
  markupLines ms (s:ss) =
    let (ms', r) = markupLine s ms in r : markupLines ms' ss

  markupLine :: (Int, T.Text) -> [Match] -> ([Match], Widget a)
  markupLine (i, s) =
    fmap (highlightLine s) . swap . span (\(Match _ _ line) -> line == i)

  highlightLine :: T.Text -> [Match] -> Widget a
  highlightLine "" _  = txt " "   -- special case for empty lines
  highlightLine s  [] = txt s     -- special case for no matches
  highlightLine s  ms = hBox $ go 0 ms
    where
    go i [] = [txt . snd $ T.splitAt i s]
    go i (m@(Match off len _line) : rest) =
      let
        (_,s')  = T.splitAt i s
        (l,s'') = T.splitAt (off - i) s'
        (c,_)   = T.splitAt len s''
      in
        txt l : withAttr (attr m) (txt c) : go (off + len) rest

  attr m
    | Just m == cur = currentTextMatchHighlightAttr
    | otherwise     = textMatchHighlightAttr
