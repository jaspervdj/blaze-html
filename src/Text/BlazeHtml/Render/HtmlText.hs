{-# LANGUAGE OverloadedStrings #-}
-- | This module exports a pure renderer that produces Text.
module Text.BlazeHtml.Render.HtmlText
    ( HtmlText
    , htmlText
    ) where

import Control.Monad.Writer
import Control.Monad.Reader

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html
import qualified Text.BlazeHtml.Text as T

newtype HtmlText = HtmlText
    { runHtmlText :: Reader [Attribute] Text
    }

-- | Simple helper function to render the attributes.
attributes :: [Attribute] -> Text
attributes [] = T.singleton ' '
attributes t  = foldr append mempty t
  where
    append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlText where
    mempty        = HtmlText $ return mempty
    mappend m1 m2 = HtmlText $ liftM2 mappend (runHtmlText m1) (runHtmlText m2)

instance Html HtmlText where
    unescapedText = HtmlText . return
    leafElement t = HtmlText $ do
        attrs <- ask
        return $ "<" `mappend` t
                     `mappend` attributes attrs `mappend` "/>"
    nodeElement t h = HtmlText $ do
        attrs <- ask
        inner <- runHtmlText $ clearAttributes h
        return $ "<" `mappend` t
                     `mappend` attributes attrs `mappend` ">"
                     `mappend` inner
                     `mappend` "</" `mappend` t `mappend` ">"
    modifyAttributeModifier f = HtmlText . local (f id) . runHtmlText

-- | Render the html to text.
htmlText :: HtmlText -> Text
htmlText = (`runReader` []) . runHtmlText
