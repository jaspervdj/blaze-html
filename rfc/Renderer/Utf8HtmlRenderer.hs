module Renderer.Utf8HtmlRenderer where

import qualified Data.ByteString.Lazy as BL

import Renderer.DefaultRenderer
import Builder.Utf8Builder
import Encoding.TotalEncoding
import Internal.Html

renderUtf8Html :: DefaultRenderer (TotalEncoding Utf8Builder) -> BL.ByteString
renderUtf8Html h = toLazyByteStringUtf8 $
    runTotalEncoding (renderDefault h) utf8tag
  where
    utf8tag = unicodeString
       "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
