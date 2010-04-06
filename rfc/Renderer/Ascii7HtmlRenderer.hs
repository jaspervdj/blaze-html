module Renderer.Ascii7HtmlRenderer where

import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Monoid (mconcat)

import Builder.Ascii7Builder
import Encoding.PartialEncoding
import Renderer.DefaultRenderer
import Internal.UnicodeSequence

-- | An ASCII-7 based encoding of a Html document.
type HtmlAscii7 = DefaultRenderer (PartialEncoding Ascii7Builder)

renderHtmlAscii7 :: HtmlAscii7 -> BL.ByteString
renderHtmlAscii7 = renderHtmlAscii7Like "US-ASCII"

-- | Encode using a character set that agrees on all first 127 characters
-- (7-bits) with the US-ASCII charset.
renderHtmlAscii7Like :: String -> HtmlAscii7 -> BL.ByteString
renderHtmlAscii7Like charset h = toLazyByteStringAscii7 $
    runPartialEncoding (renderDefault h) info unicodeChar
  where
    info = EncodingInfo
        { eiTag       = mconcat . map unicodeString $
          [ "<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
          , charset
          , "/>" 
          ]
        , eiUnencodable = (\c -> 0x7F < ord c)
        }
