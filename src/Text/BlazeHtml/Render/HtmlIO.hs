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

import Data.Monoid
import Data.Text (Text)

import Text.BlazeHtml.Internal.Html

type TextOutputter = Text -> IO ()

-- | A html document that gets output over IO.
newtype HtmlIO = HtmlIO {getHtmlIO :: TextOutputter -> Attributes -> IO ()}

-- | Output an HtmlIO value using the given text output function.
renderHtmlIO :: (Text -> IO ()) -> HtmlIO -> IO ()
renderHtmlIO out h = getHtmlIO h out []

-- | Helper function to render attributes.
renderUnescapedAttributes :: TextOutputter -> Attributes -> IO ()
renderUnescapedAttributes out = mapM_ $ \(k,v) -> 
    out " " >> out k >> out "=\"" >> out v >> out "\""

-- | Render a begin tag except for its end.
renderBeginTag :: Text -> TextOutputter -> Attributes -> IO ()
renderBeginTag t out attrs =
    out "<" >> out t >> renderUnescapedAttributes out attrs

instance Monoid HtmlIO where
    mempty        = HtmlIO . const . const $ return ()
    mappend h1 h2 = HtmlIO $ \out attrs -> 
        getHtmlIO h1 out attrs >> getHtmlIO h2 out attrs

instance Html HtmlIO where
    renderUnescapedText t = HtmlIO $ \out _ -> out t
    renderLeafElement t   = HtmlIO $ \out attrs ->
        renderBeginTag t out attrs >> out "/>"
    modifyUnescapedAttributes f h = HtmlIO $ \out attrs -> 
        getHtmlIO h out (f attrs)
    renderElement t h = HtmlIO $ \out attrs -> do
        renderBeginTag t out attrs >> out ">"
        getHtmlIO h out []
        out "</" >> out t >> out ">"
