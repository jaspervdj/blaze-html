{-# LANGUAGE OverloadedStrings #-}
-- | This module exports a pure renderer that produces Text.
module Text.BlazeHtml.Render.HtmlPrettyText
    ( HtmlPrettyText
    , renderHtmlPrettyText
    ) where

import Prelude hiding (replicate)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text,replicate)
import qualified Data.Text as T

import Text.BlazeHtml.Internal.Html

type IndentLevel = Int

newtype HtmlPrettyText = HtmlPrettyText
    { runHtmlPrettyText :: StateT IndentLevel (Reader Attributes) Text
    }

-- | Simple helper function to render the attributes.
renderAttributes :: Attributes -> Text
renderAttributes [] = T.empty
renderAttributes t  = foldr append mempty $ t
  where
    append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlPrettyText where
    mempty        = HtmlPrettyText $ return mempty
    mappend m1 m2 = HtmlPrettyText $ liftM2 mappend (runHtmlPrettyText m1)
                                                    (runHtmlPrettyText m2)

instance Html HtmlPrettyText where
    renderUnescapedText = HtmlPrettyText . return
    renderLeafElement t = HtmlPrettyText $ do
        attrs <- ask
        indent <- get
        return $ replicate indent " " `mappend` "<" `mappend` t
                                      `mappend` renderAttributes attrs
                                      `mappend` "/>\n"
    modifyUnescapedAttributes f =
        HtmlPrettyText . local (f id) . runHtmlPrettyText
    renderElement t h = HtmlPrettyText $ do
        attrs <- ask
        indent <- get
        put (indent + 2)
        inner <- runHtmlPrettyText $ clearAttributes h
        put indent
        return $ "<" `mappend` t
                     `mappend` renderAttributes attrs `mappend` ">\n"
                     `mappend` inner
                     `mappend` "</" `mappend` t `mappend` ">\n"

-- | Render the html to text.
renderHtmlPrettyText :: HtmlPrettyText -> Text
renderHtmlPrettyText x =
    (`runReader` []) $ liftM fst $ runStateT (runHtmlPrettyText x) 0
