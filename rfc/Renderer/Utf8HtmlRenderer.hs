module Renderer.Utf8HtmlRenderer where

import Data.Monoid (Monoid, mempty)

import Renderer.DefaultRenderer
import Builder.Utf8Builder
import Encoding.TotalEncoding

renderUtf8Html :: DefaultRenderer (TotalEncoding Utf8Builder) -> BL.ByteString
renderUtf8Html h = toLazyByteStringUtf8 (runTE (renderDefault h) utf8tag)
  where
    utf8tag = unicodeString
       "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
