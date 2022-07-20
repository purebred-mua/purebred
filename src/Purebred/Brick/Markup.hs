-- This module was copied from brick-0.68 and authored by Jonathan
-- Daugherty <cygnus@foobox.com>
-- | This module provides an API for turning "markup" values into
-- widgets. This module uses the "Data.Text.Markup" interface in this
-- package to assign attributes to substrings in a text string; to
-- manipulate markup using (for example) syntax highlighters, see that
-- module.
module Purebred.Brick.Markup
  ( Markup
  , markup
  , (@?)
  , GetAttr(..)
  )
where

import Control.Lens ((.~), (&), (^.))
import Control.Monad (forM)
import qualified Data.Text as T
import Data.Text.Markup

import Graphics.Vty (Attr, vertCat, horizCat, text', defAttr)

import Brick.AttrMap
import Brick.Types

-- | A type class for types that provide access to an attribute in the
-- rendering monad.  You probably won't need to instance this.
class GetAttr a where
    -- | Where to get the attribute for this attribute metadata.
    getAttr :: a -> RenderM n Attr

instance GetAttr Attr where
    getAttr a = do
        c <- getContext
        return $ mergeWithDefault a (c^.ctxAttrMapL)

instance GetAttr AttrName where
    getAttr = lookupAttrName

-- | Build a piece of markup from text with an assigned attribute name.
-- When the markup is rendered, the attribute name will be looked up in
-- the rendering context's 'AttrMap' to determine the attribute to use
-- for this piece of text.
(@?) :: T.Text -> AttrName -> Markup AttrName
(@?) = (@@)

-- | Build a widget from markup.
markup :: (Eq a, GetAttr a) => Markup a -> Widget n
markup m =
    Widget Fixed Fixed $ do
      let markupLines = markupToList m
          mkLine pairs = do
              is <- forM pairs $ \(t, aSrc) -> do
                  a <- getAttr aSrc
                  return $ text' a t
              if null is
                 then do
                     def <- getAttr defAttr
                     return $ text' def $ T.singleton ' '
                 else return $ horizCat is
      lineImgs <- mapM mkLine markupLines
      return $ emptyResult & imageL .~ vertCat lineImgs
