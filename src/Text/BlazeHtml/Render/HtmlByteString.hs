{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.Render.HtmlByteString
    ( HtmlByteString
    , renderHtmlByteString
    , buildHtmlByteString
    ) where

import Data.ByteString.Lazy as LB hiding (map)
import Data.Binary.Builder
import Data.Monoid
import Text.BlazeHtml.Text (Text, textToBuilder)

import Text.BlazeHtml.Internal.Html

-- | A html document that concatenates text in a builder.
newtype HtmlByteString = HtmlByteString
    { getHtmlByteString :: [Attribute] -> Builder
    }

-- | Output an HtmlByteString value using the given text output function.
renderHtmlByteString :: HtmlByteString -> LB.ByteString
renderHtmlByteString = toLazyByteString . buildHtmlByteString

buildHtmlByteString :: HtmlByteString -> Builder
buildHtmlByteString = ($ []) . getHtmlByteString

-- | Helper function to render attributes.
renderUnescapedAttributes :: [Attribute] -> Builder
renderUnescapedAttributes attrs = mconcat $ flip map attrs $ \(k,v) -> 
    textToBuilder " " `mappend` textToBuilder k
                      `mappend` textToBuilder "=\""
                      `mappend` textToBuilder v
                      `mappend` textToBuilder "\""

-- | Render a begin tag except for its end.
renderBeginTag :: Text -> [Attribute] -> Builder
renderBeginTag tag attrs =
    textToBuilder "<" `mappend` textToBuilder tag
                      `mappend` renderUnescapedAttributes attrs

instance Monoid HtmlByteString where
    mempty        = HtmlByteString $ const mempty
    mappend h1 h2 = HtmlByteString $ \attrs -> 
        getHtmlByteString h1 attrs `mappend` getHtmlByteString h2 attrs

instance Html HtmlByteString where
    renderUnescapedText = HtmlByteString . const . textToBuilder
    renderLeafElement t   = HtmlByteString $ \attrs -> 
        renderBeginTag t attrs `mappend` textToBuilder "/>"
    modifyUnescapedAttributes f html = HtmlByteString $ \attrs ->
        getHtmlByteString html (f id attrs)
    renderElement t html = HtmlByteString $ \attrs ->
        renderBeginTag t attrs `mappend` textToBuilder ">"
                               `mappend` getHtmlByteString html []
                               `mappend` textToBuilder "</"
                               `mappend` textToBuilder t
                               `mappend` textToBuilder ">"
