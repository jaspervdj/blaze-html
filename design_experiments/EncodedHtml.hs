{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module EncodedHtml where

import           Prelude             hiding (head)

import           Control.Exception          (assert)

import qualified Data.ByteString.Lazy as BL
import           Data.Char                  (ord, chr)
import           Data.List                  (foldl')
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T  (decodeUtf8)
import qualified Data.Text.IO         as T  (putStrLn)
import           Data.Word

import           Debug.Trace                (trace)

import           Numeric                    (showHex)

import UnicodeSequence



-----------------------------------------------------------------------------
-- A type-class for abstracting Html documents
-----------------------------------------------------------------------------


class UnicodeSequence h => Encoded h where
    -- | The tag marking the encoding
    encodingTag        :: h
    -- | Replace all unencodable characters using the given character
    -- substitution.
    replaceUnencodable :: (Char -> h) -> h -> h

class Encoded h => Html h where
    -- | Left html, right html
    separate     :: h -> h -> h
    -- | Tag
    leafElement  :: h -> h
    -- | Tag, inner html
    nodeElement  :: h -> h -> h
    -- | Key, value, html taking attributes
    addAttribute :: h -> h -> h -> h

unescapedChar :: UnicodeSequence s => Char -> s
unescapedChar = unicodeChar

unescapedText :: UnicodeSequence s => Text -> s
unescapedText = unicodeText

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


------------------------------------------------------------------------------
-- A Flattener
------------------------------------------------------------------------------

-- | represent a Html document as a single line without line-breaks.
newtype OneLineHtml s = OLH {runOLH :: s -> s}
    deriving( UnicodeSequence )

instance Monoid m => Monoid (OneLineHtml m) where
    mempty = OLH $ const mempty
    (OLH m1) `mappend` (OLH m2) = OLH $ m1 `mappend` m2
    -- faster, as attributes are not threaded through the sequence of mappends,
    -- as they would for the default implementation of mconcat
    --
    -- NOTE suggest addition of the corresponding definition to the stantard
    -- libraries.
    mconcat hs = OLH $ \attrs -> mconcat $ map (`runOLH` attrs) hs

instance Encoded s => Encoded (OneLineHtml s) where
    encodingTag = OLH $ const encodingTag
    replaceUnencodable subst inner = OLH $ \attrs -> 
        replaceUnencodable ((`runOLH` attrs) . subst) (runOLH inner attrs)

instance (Encoded s, UnicodeSequence s) => Html (OneLineHtml s) where
    h1 `separate` h2 = OLH $ \attrs -> mconcat
        [ runOLH h1 attrs 
        , unicodeChar ' '
        , runOLH h2 attrs
        ]
    leafElement tag = OLH $ \attrs -> mconcat
        [ unicodeChar '<'
        , runOLH tag mempty
        , attrs
        , unicodeString "/>"
        ]
    nodeElement tag inner = OLH $ \attrs -> mconcat
        [ unicodeChar '<'
        , runOLH tag mempty
        , attrs
        , unicodeChar '>'
        , runOLH inner mempty
        , unicodeString "</"
        , runOLH tag mempty
        , unicodeChar '>'
        ]
    addAttribute key value h = OLH $ \attrs -> 
        runOLH h $ mconcat
            [ unicodeChar ' '
            , runOLH key mempty
            , unicodeChar '='
            , unicodeChar '"'
            , runOLH value mempty
            , unicodeChar '"'
            ] `mappend` attrs

------------------------------------------------------------------------------
-- An Encoded instance for TOTAL encodings
------------------------------------------------------------------------------

-- | An encoding that never needs to replace a character; eg.g. UTF-8, UTF-16,
-- and UTF-32.
newtype TotalEncoding s = TE { runTE :: s -> s }
    deriving( Monoid, UnicodeSequence )

instance UnicodeSequence s => Encoded (TotalEncoding s) where
    encodingTag        = TE id
    replaceUnencodable = const id

runHtmlUtf8 :: OneLineHtml (TotalEncoding Utf8Builder) -> BL.ByteString
runHtmlUtf8 h = toLazyByteStringUtf8 (runTE (runOLH h mempty) utf8tag)
  where
    utf8tag = unicodeString
       "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"

------------------------------------------------------------------------------
-- An Encoded instance for Partial encodings
------------------------------------------------------------------------------

data EncodingInfo s = EncodingInfo {
      eiTag         :: !s
    , eiUnencodable :: !(Char -> Bool)
    }

newtype PartialEncoding s = PE { runPE :: EncodingInfo s -> (Char -> s) -> s }

instance Monoid s => Monoid (PartialEncoding s) where
    mempty          = PE $ \ _ _     -> mempty
    s1 `mappend` s2 = PE $ \ei subst -> 
        runPE s1 ei subst `mappend` runPE s2 ei subst
    mconcat ss      = PE $ \ei subst -> 
        mconcat . map (\s -> runPE s ei subst) $ ss

instance UnicodeSequence s => UnicodeSequence (PartialEncoding s) where
    unicodeChar c   = PE $ \ei subst ->
        if eiUnencodable ei c
            then subst c
            else unicodeChar c
    unicodeText     = mconcat . map unicodeChar . T.unpack

instance UnicodeSequence s => Encoded (PartialEncoding s) where
    encodingTag                     = PE $ \ei _ -> eiTag ei
    replaceUnencodable subst' inner = PE $ \ei subst -> 
        runPE inner ei (\c -> runPE (subst' c ) ei subst)

-- | Compute the sequence of unicode characters representing the given integer
-- in base 16.
unicodeHexInt :: UnicodeSequence s => Int -> s
unicodeHexInt n = unicodeString $ showHex n ""
-- FIXME: specialize showHex for max speed and such that underlying character
-- constants are shared as good as possible.

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

-- A Latin-1 (IS0 8859-1) encoded Html document
-----------------------------------------------

-- TODO: For specialization to work nicely it may be required to build
-- a separate type-class instance for Encoded with fixed values for
-- the info.
type HtmlLatin1 = OneLineHtml (PartialEncoding Latin1Builder) 

runHtmlLatin1 :: HtmlLatin1 -> BL.ByteString
runHtmlLatin1 h = 
  toLazyByteStringLatin1 (runPE (runOLH h mempty) info unicodeChar)
  where
    info = EncodingInfo {
        eiTag         = unicodeString
          "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>"
      , eiUnencodable = (\c -> 0xFF < ord c)
      }


-- Generic ASCII like encodings
-------------------------------

-- | An ASCII-7 based encoding of a Html document.
type HtmlAscii7 = OneLineHtml (PartialEncoding Ascii7Builder)

runHtmlAscii7 :: HtmlAscii7 -> BL.ByteString
runHtmlAscii7 = runHtmlAscii7Like "US-ASCII"

-- | Encode using a character set that agrees on all first 127 characters
-- (7-bits) with the US-ASCII charset.
runHtmlAscii7Like :: String -> HtmlAscii7 -> BL.ByteString
runHtmlAscii7Like charset h = 
  toLazyByteStringAscii7 (runPE (runOLH h mempty) info unicodeChar)
  where
    info = EncodingInfo {
        eiTag         = mconcat . map unicodeString $
          [ "<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
          , charset
          , "/>" 
          ]
      , eiUnencodable = (\c -> 0x7F < ord c)
      }


------------------------------------------------------------------------------
-- Testing the sharing; i.e. it is hard to get generically... :-(
------------------------------------------------------------------------------

-- It seems that only the above formulation achieves the expected sharing
-- of the static strings among different calls.
testShared :: Char -> Utf8Builder
testShared = htmlCharReference

-- It seems that only the above formulation achieves the expected sharing
-- of the static strings among different calls.
testUnShared :: Char -> Utf8Builder
testUnShared = jsCharReference


------------------------------------------------------------------------------
-- A UnicodeSequence Transformer for Simple One-Character Escaping
------------------------------------------------------------------------------

data EscapingInfo s = EscapingInfo {
      escapeChar :: !(Char  -> s)
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

------------------------------------------------------------------------------
-- Combinators for building properly escaped and encoded content
------------------------------------------------------------------------------

htmlContent :: Encoded s => Unescaped s -> s
htmlContent = replaceUnencodable htmlCharReference . escapeHtmlContent

string :: Encoded s => String -> s
string = htmlContent . unicodeString

text :: Encoded s => Text -> s
text = htmlContent . unicodeText

head :: Html h => h -> h
head inner = nodeElement (unicodeString "head") (encodingTag `mappend` inner)

-- | c.f. HTML 4.01 Standard, Section 6.2
script :: Html h => h -> h
script = 
    nodeElement (unicodeString "script") . replaceUnencodable jsCharReference

-- | c.f. HTML 4.01 Standard, Section 6.2
style :: Html h => h -> h
style = 
    nodeElement (unicodeString "style") . replaceUnencodable cssCharReference

jsString :: (Encoded s) => Unescaped s -> s
jsString s = mconcat 
    [ unicodeChar '\''
    , replaceUnencodable jsCharReference $ escapeSingleQuotedJSString s
    , unicodeChar '\'' ]

-- TODO
cssString = undefined

testString :: UnicodeSequence s => s
testString = unicodeString "hello\"'\\'\" world └─╼"

testJS :: Encoded s => s
testJS = jsString testString

testAtt :: Encoded s => s
testAtt = mconcat $ 
    [ unicodeString "alert(", testJS, unicodeString ");" ]

displayTE :: TotalEncoding Utf8Builder -> IO ()
displayTE s = displayUtf8 $ runTE s (unicodeString "<TEST-ENCODING-TAG>")

html :: Html h => h -> h
html = nodeElement (unicodeString "html")

testDoc :: Html h => h
testDoc = html $ head ! attribute "onload" testAtt $ htmlContent testString

testDocUtf8 = runHtmlUtf8 testDoc
testDocLatin1 = runHtmlLatin1 testDoc

------------------------------------------------------------------------------
-- Attributes
------------------------------------------------------------------------------

instance Encoded h => Encoded (h -> h) where
    encodingTag = const encodingTag
    replaceUnencodable subst h = 
        const $ replaceUnencodable (\c -> subst c mempty) (h mempty)

-- We require this instance for allowing to addAttributes to nesting
-- html combinators; i.e. combinators of type 'h -> h'. Except for
-- the third argument of 'addAttribute' the outer argument is not
-- passed through. This is to avoid unnecessary references to these
-- arguments.
--
-- TODO: Check what inlining (and if required specialization) is necessary to
-- get rid of the abstraction cost.
instance Html h => Html (h -> h) where
    h1 `separate` h2    = const $ h1 mempty `separate` h2 mempty
    leafElement h       = const $ h mempty
    nodeElement h inner = const $ nodeElement (h mempty) (inner mempty)
    addAttribute key value h = \inner ->
        addAttribute (key mempty) (value mempty) (h inner)

newtype Attribute h = Attribute (h -> h)

-- | Construct an attribute with proper escaping and character replacement for
-- unencodable characters.
attribute :: Html h => String -> Unescaped h -> Attribute h
attribute k v = Attribute $ addAttribute key val
  where
    key = unicodeString k
    val = replaceUnencodable htmlCharReference (escapeDoubleQuotedAttribute v)


(!) :: Html h => h -> Attribute h -> h
h ! Attribute f = f h

(<!) :: Html h => h -> [Attribute h] -> h
h <! atts = foldl' (!) h atts

