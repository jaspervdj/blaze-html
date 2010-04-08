module Internal.Attributes where

import Data.List (foldl')

import Data.Text (Text)

import Internal.EncodedHtml
import Internal.Escaping
import Internal.UnicodeSequence (unicodeText)

newtype Attribute h = Attribute (h -> h)

-- | Construct an attribute with proper escaping and character replacement for
-- unencodable characters.
attribute :: Html h => Text -> Unescaped h -> Attribute h
attribute k v = Attribute $ addAttribute key val
  where
    key = unicodeText k
    val = replaceUnencodable htmlCharReference (escapeDoubleQuotedAttribute v)

(!) :: Html h => h -> Attribute h -> h
h ! Attribute f = f h

(<!) :: Html h => h -> [Attribute h] -> h
h <! atts = foldl' (!) h atts
