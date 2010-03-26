{-# LANGUAGE OverloadedStrings #-}
-- | An instance of the Html typeclass modelling html documents that get output
-- directly using IO.
--
-- TODO: Add instructions explaining how to get high-performance when using
-- this document type. Most likely this amounts to a simple SPEZIALIZE pragma
-- for each function producing values of the 'HtmlIO' type.
module Text.BlazeHtml.Render.HtmlIO
    ( HtmlIO
    , htmlIO
    ) where

import Prelude hiding (putStr)
import Data.Monoid
import System.IO (Handle)
import Control.Monad (forM_)

import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (Text,hPutStr)

--type TextOutputter = Text -> IO ()

-- | A html document that gets output over IO.
newtype HtmlIO = HtmlIO {getHtmlIO :: Handle -> [Attribute] -> IO ()}

-- | Output an HtmlIO value using the given text output function.
htmlIO :: Handle -> HtmlIO -> IO ()
htmlIO h html = getHtmlIO html h []

-- | Helper function to render attributes.
attributes :: Handle -> [Attribute] -> IO ()
attributes h [] = hPutStr h " "
attributes h attrs = forM_ attrs $ \(k,v) -> 
    hPutStr h " " >> hPutStr h k >> hPutStr h "=\""
                  >> hPutStr h v >> hPutStr h "\""

-- | Render a begin tag except for its end.
beginTag :: Handle -> Text -> [Attribute] -> IO ()
beginTag h t attrs = hPutStr h "<" >> hPutStr h t >> attributes h attrs

instance Monoid HtmlIO where
    mempty        = HtmlIO . const . const $ return ()
    mappend h1 h2 = HtmlIO $ \out attrs -> 
        getHtmlIO h1 out attrs >> getHtmlIO h2 out attrs

instance Html HtmlIO where
    unescapedText t = HtmlIO $ \h _ -> hPutStr h t
    leafElement t   = HtmlIO $ \h attrs -> 
        beginTag h t attrs >> hPutStr h "/>"
    nodeElement t htmlio = HtmlIO $ \h attrs -> do
        beginTag h t attrs >> hPutStr h ">"
        getHtmlIO htmlio h []
        hPutStr h "</" >> hPutStr h t >> hPutStr h ">"
    modifyAttributeModifier f htmlio = HtmlIO $ \h -> getHtmlIO htmlio h . f id 
