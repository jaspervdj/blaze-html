{-# LANGUAGE OverloadedStrings #-}
-- | An instance of the Html typeclass modelling html documents that get output
-- directly using IO.
--
-- TODO: Add instructions explaining how to get high-performance when using
-- this document type. Most likely this amounts to a simple SPEZIALIZE pragma
-- for each function producing values of the 'HtmlIO' type.
module Text.BlazeHtml.Render.HtmlIO
    ( HtmlIO
    , renderHtmlIO
    ) where

import Prelude hiding (putStr)
import Data.Monoid
import Text.BlazeHtml.Text (Text,hPutStr)
import System.IO (Handle)

import Text.BlazeHtml.Internal.Html

--type TextOutputter = Text -> IO ()

-- | A html document that gets output over IO.
newtype HtmlIO = HtmlIO {getHtmlIO :: Handle -> [Attribute] -> IO ()}

-- | Output an HtmlIO value using the given text output function.
renderHtmlIO :: Handle -> HtmlIO -> IO ()
renderHtmlIO h html = getHtmlIO html h []

-- | Helper function to render attributes.
renderUnescapedAttributes :: Handle -> [Attribute] -> IO ()
renderUnescapedAttributes h = mapM_ $ \(k,v) -> 
    hPutStr h " " >> hPutStr h k >> hPutStr h "=\"" >> hPutStr h v >> hPutStr h "\""

-- | Render a begin tag except for its end.
renderBeginTag :: Handle -> Text -> [Attribute] -> IO ()
renderBeginTag h t attrs =
    hPutStr h "<" >> hPutStr h t >> renderUnescapedAttributes h attrs

instance Monoid HtmlIO where
    mempty        = HtmlIO . const . const $ return ()
    mappend h1 h2 = HtmlIO $ \out attrs -> 
        getHtmlIO h1 out attrs >> getHtmlIO h2 out attrs

instance Html HtmlIO where
    renderUnescapedText t = HtmlIO $ \h _ -> hPutStr h t
    renderLeafElement t   = HtmlIO $ \h attrs -> 
        renderBeginTag h t attrs >> hPutStr h "/>"
    modifyUnescapedAttributes f htmlio = HtmlIO $ \h -> getHtmlIO htmlio h . f id 
    renderElement t htmlio = HtmlIO $ \h attrs -> do
        renderBeginTag h t attrs >> hPutStr h ">"
        getHtmlIO htmlio h []
        hPutStr h "</" >> hPutStr h t >> hPutStr h ">"
