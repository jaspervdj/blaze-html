{-# LANGUAGE OverloadedStrings #-}
-- | This module exports a pure renderer that produces Text.
module Text.BlazeHtml.Render.HtmlPrettyText
    ( HtmlPrettyText
    , htmlPrettyText
    ) where

import Prelude hiding (replicate)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html
import qualified Text.BlazeHtml.Text as T

type IndentLevel = Int

newtype HtmlPrettyText = HtmlPrettyText
    { runHtmlPrettyText :: StateT IndentLevel (Reader [Attribute]) Text
    }

-- | Simple helper function to render the attributes.
attributes :: [Attribute]-> Text
attributes [] = T.singleton ' '
attributes t  = foldr append mempty t
  where
    append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlPrettyText where
    mempty        = HtmlPrettyText $ return mempty
    mappend m1 m2 = HtmlPrettyText $ liftM2 mappend (runHtmlPrettyText m1)
                                                    (runHtmlPrettyText m2)

instance Html HtmlPrettyText where
    unescapedText = HtmlPrettyText . return
    leafElement t = HtmlPrettyText $ do
        attrs <- ask
        indent <- get
        return $ T.replicate indent " " `mappend` "<" `mappend` t
                                        `mappend` attributes attrs
                                        `mappend` "/>\n"
    nodeElement t h = HtmlPrettyText $ do
        attrs <- ask
        indent <- get
        put (indent + 2)
        inner <- runHtmlPrettyText $ clearAttributes h
        put indent
        return $ "<" `mappend` t
                     `mappend` attributes attrs `mappend` ">\n"
                     `mappend` inner
                     `mappend` "</" `mappend` t `mappend` ">\n"
    modifyAttributes f =
        HtmlPrettyText . local (f id) . runHtmlPrettyText

-- | Render the html to text.
htmlPrettyText :: HtmlPrettyText -> Text
htmlPrettyText x =
    (`runReader` []) $ liftM fst $ runStateT (runHtmlPrettyText x) 0
