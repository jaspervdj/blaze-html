module Renderer.Latin1HtmlRenderer where

import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)

import Builder.Latin1Builder
import Encoding.PartialEncoding
import Renderer.DefaultRenderer
import Internal.UnicodeSequence

-- TODO: For specialization to work nicely it may be required to build
-- a separate type-class instance for Encoded with fixed values for
-- the info.
type HtmlLatin1 = DefaultRenderer (PartialEncoding Latin1Builder) 

renderHtmlLatin1 :: HtmlLatin1 -> BL.ByteString
renderHtmlLatin1 h = toLazyByteStringLatin1 $
    runPartialEncoding (renderDefault h) info unicodeChar
  where
    info = EncodingInfo
        { eiTag = unicodeString tag
        , eiUnencodable = (\c -> 0xFF < ord c)
        }

    tag = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>"
