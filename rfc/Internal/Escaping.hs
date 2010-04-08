{-# LANGUAGE FlexibleInstances #-}
module Internal.Escaping where

import Data.Monoid (Monoid, mconcat, mempty, mappend)
import Data.Char (ord)
import Numeric (showHex)

import GHC.Exts (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T

import Internal.Html

-- | Here we want to ensure that standard Html entity escaping is done
-- and if required encoding dependent translation also.
elementText :: Html h => Text -> h
elementText = replaceUnencodable undefined . undefined

-- | Here we want to ensure that standard Html entity escaping is done
-- and if required encoding dependent.
attributeText :: Html h => Text -> h
attributeText = undefined

-- | Here, we want to ensure that every char <= 255 is replaced by
-- \xXX and if required every char >= is also replaced by \xXXX.
--
-- If the Html parser runs first, why should attribute escaping not work?
javaScriptText :: Html h => Text -> h
javaScriptText = replaceUnencodable jsEscapeChar . undefined
  where
    jsEscapeChar = undefined

-- | Compute the sequence of unicode characters representing the given integer
-- in base 16.
unicodeHexInt :: UnicodeSequence s => Int -> s
unicodeHexInt n = unicodeString $ showHex n ""
-- FIXME: specialize showHex for max speed and such that underlying character
-- constants are shared as good as possible. This function will be used quite a
-- bit when converting a Html document to a ASCII7 encoded bytestream.

-- | Build a HTML character reference using hexadecimal notation.
htmlCharReference :: UnicodeSequence s => Char -> s
htmlCharReference = \c -> mconcat [pre , unicodeHexInt $ ord c , post]
  where
    pre  = unicodeString "&#"
    post = unicodeChar ';' 

-- | Build a JavaScript character reference using hexadecimal notation.
jsCharReference :: UnicodeSequence s => Char -> s
jsCharReference c = mconcat [unicodeString "\\x", unicodeHexInt $ ord c]

-- | Build a URL character reference using hexadecimal notation.
cssCharReference :: UnicodeSequence s => Char -> s
cssCharReference c = mconcat [unicodeChar '\\', unicodeHexInt $ ord c]

-- | Build a URL character reference using hexadecimal notation.
urlCharReference :: UnicodeSequence s => Char -> s
urlCharReference c = mconcat [unicodeChar '%', unicodeHexInt $ ord c ]

-- | A UnicodeSequence Transformer for Simple One-Character Escaping
data EscapingInfo s = EscapingInfo
    { escapeChar :: !(Char  -> s)
    , escapeText :: !(Text  -> s)
    }

newtype Unescaped s = Unescaped { escape :: EscapingInfo s -> s }

instance Monoid s => Monoid (Unescaped s) where
    mempty          = Unescaped $ const mempty
    s1 `mappend` s2 = Unescaped $ \ei -> escape s1 ei `mappend` escape s2 ei
    mconcat ss      = Unescaped $ \ei -> mconcat . map (`escape` ei) $ ss

instance Monoid s => UnicodeSequence (Unescaped s) where
    unicodeChar c = Unescaped $ \info -> escapeChar info c
    unicodeText t = Unescaped $ \info -> escapeText info t

instance Encoded s => Encoded (Unescaped s) where
    encodingTag                    = Unescaped $ const encodingTag
    replaceUnencodable subst inner = Unescaped $ \ei ->
        replaceUnencodable (\c -> escape (subst c) ei) (escape inner ei)

-- | Escape nothing; needed for some tricks like displaying the result of a
-- Html document as well as the code.
escapeNothing :: UnicodeSequence s => Unescaped s -> s
escapeNothing = (`escape` EscapingInfo unicodeChar unicodeText)

-- | Escape HTML content.
escapeHtmlContent :: UnicodeSequence s => Unescaped s -> s
escapeHtmlContent = (`escape` EscapingInfo escChar escText)
    where
    escChar c = case c of
        '&'  -> unicodeString "&amp;"
        '<'  -> unicodeString "&lt;"
        '>'  -> unicodeString "&gt;"
        '"'  -> unicodeString "&quot;"
        '\'' -> unicodeString "&#x27;"  -- &apos; is not recommended
        '/'  -> unicodeString "&#x2F;"  -- forward slash is included as it
                                        -- helps end an HTML entity
        _    -> unicodeChar c
    -- TODO: faster implementation based on text internal representation
    escText   = mconcat . map escChar . T.unpack
    -- TODO: enable better sharing of escaping strings

-- | Escape a doublequoted HTML attribute value.
--
-- NOTE: If the attribute is further parsed you need to take care that its
-- value is also properly escaped with respect to this inner parser before
-- escaping it w.r.t. the attribute parser.
escapeDoubleQuotedAttribute :: UnicodeSequence s => Unescaped s -> s
escapeDoubleQuotedAttribute = (`escape` EscapingInfo escChar escText)
    where
    escChar c = case c of
        '&'  -> amp
        '"'  -> dquote
        _    -> unicodeChar c
    -- TODO: faster implementation based on text internal representation
    escText   = mconcat . map escChar . T.unpack
    -- achieving sharing across multiple calls of escChar
    amp    = unicodeString "&amp;"
    dquote = unicodeString "&quot;"


-- | Escape for enclosing in a single quoted JavaScript string.
escapeSingleQuotedJSString :: UnicodeSequence s => Unescaped s -> s
escapeSingleQuotedJSString = (`escape` EscapingInfo escChar escText)
    where
    escChar c = case c of
        '\\' -> backslash
        '\'' -> apos
        '/'  -> slash           -- required because </ ends 
        '<'  -> langle          -- a <style> or <script> content
        _    -> unicodeChar c
    -- TODO: faster implementation based on text internal representation
    escText   = mconcat . map escChar . T.unpack
    -- achieving sharing across multiple calls of escChar
    backslash = unicodeString "\\\\"
    apos      = unicodeString "\\x27"
    slash     = unicodeString "\\x2F"  
    langle    = unicodeString "\\x3C"  

-- | Paranoid escaping of all characters below 0xFF that are not alpha-numeric.
-- This is what the ESAPI project recommends in their XSS article:
-- http://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet
-- 
-- We are currently using it for CSS and URL escaping, as there I do not yet
-- know what is required to make sure that the parser does not make any
-- misinterpretation.
-- 
-- TODO: Investigate this and check if a more bandwidth efficient escaping is
-- possible.
escapeParanoid :: UnicodeSequence s => (Char -> s) -> Unescaped s -> s
escapeParanoid charRef = (`escape` EscapingInfo escChar escText)
    where
    escChar c
        |    ('0' <= c && c <= '9')
          || ('A' <= c && c <= 'Z')
          || ('a' <= c && c <= 'z') = unicodeChar c
        | otherwise                 = charRef c
    -- TODO: faster implementation based on text internal representation
    escText   = mconcat . map escChar . T.unpack

escapeCss :: UnicodeSequence s => Unescaped s -> s
escapeCss = escapeParanoid cssCharReference

escapeUrl :: UnicodeSequence s => Unescaped s -> s
escapeUrl = escapeParanoid urlCharReference
