{-# LANGUAGE OverloadedStrings #-}
-- | This module exports a pure renderer that produces Text.
module Text.BlazeHtml.Render.HtmlPretty
    ( HtmlPretty
    , renderHtmlPretty
    ) where

import Prelude hiding (replicate)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text,replicate)
import qualified Data.Text as T

import Text.BlazeHtml.Internal.Html

type IndentLevel = Int

newtype HtmlPretty = HtmlPretty 
    { runHtmlPretty :: StateT IndentLevel (Reader Attributes) Text
    }

-- | Simple helper function to render the attributes.
renderAttributes :: Attributes -> Text
renderAttributes [] = T.empty
renderAttributes t  = foldr append mempty $ t
  where
    append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlPretty where
    mempty        = HtmlPretty $ return mempty
    mappend m1 m2 = HtmlPretty $ liftM2 mappend (runHtmlPretty m1) (runHtmlPretty m2)

instance Html HtmlPretty where
    renderUnescapedText = HtmlPretty . return
    renderLeafElement t = HtmlPretty $ do
        attrs <- ask
        indnt <- get
        return $ (replicate indnt "  ") `mappend` "<" `mappend` t
                     `mappend` renderAttributes attrs `mappend` "/>\n"
    modifyUnescapedAttributes f = HtmlPretty . local f . runHtmlPretty
    renderElement t h = HtmlPretty $ do
        attrs <- ask
        indnt <- get
        put (indnt+1)
        inner <- runHtmlPretty (modifyUnescapedAttributes (const []) h)
        put indnt
        return $ "<" `mappend` t
                     `mappend` renderAttributes attrs `mappend` ">\n"
                     `mappend` inner
                     `mappend` "</" `mappend` t `mappend` ">\n"

-- | Render the html to text.
renderHtmlPretty :: HtmlPretty -> Text
renderHtmlPretty x = (`runReader` []) $ liftM fst $ runStateT (runHtmlPretty x) 0
