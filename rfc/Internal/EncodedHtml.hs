{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | Encoding and rendering independent first-class values for representing
-- Html and other tree-like documents.
module Internal.EncodedHtml where

import Prelude hiding (head)
import Data.Monoid (mempty)

import Data.Text (Text)

import Internal.UnicodeSequence

-- | A typeclass represeting a certain encoding.
class UnicodeSequence h => Encoded h where
    -- | The tag marking the encoding
    encodingTag        :: h
    -- | Replace all unencodable characters using the given character
    -- substitution.
    replaceUnencodable :: (Char -> h) -> h -> h

-- | The main Html typeclass.
class Encoded h => Html h where
    -- | Left html, right html
    separate     :: h -> h -> h
    -- | Tag
    leafElement  :: h -> h
    -- | Tag, inner html
    nodeElement  :: h -> h -> h
    -- | Key, value, html taking attributes
    addAttribute :: h -> h -> h -> h

-- | Put a character without doing escaping.
unescapedChar :: UnicodeSequence s => Char -> s
unescapedChar = unicodeChar

-- | Put some text without doing escaping.
unescapedText :: UnicodeSequence s => Text -> s
unescapedText = unicodeText

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

------------------------------------------------------------------------------
-- Combinators for building properly escaped and encoded content
------------------------------------------------------------------------------

{-
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

testString :: UnicodeSequence s => s
testString = unicodeString "hello\"'\\'\" world └─╼"

testJS :: Encoded s => s
testJS = jsString testString

testAtt :: Encoded s => s
testAtt = mconcat $ 
    [ unicodeString "alert(", testJS, unicodeString ");" ]

displayTE :: TotalEncoding Utf8Builder -> IO ()
displayTE s = displayUtf8 $ runTE s (unicodeString "<TEST-ENCODING-TAG>")

-- | The html combinator for HTML 4.01 strict. 
--
-- This conforms to the first milestone of BlazeHtml to achieve a complete set
-- of combinators for creating encoding independent HTML 4.01 strict documents
-- with the guarantee that all created documents are well-formed and parsed
-- back into "the same" structure by a HTML 4.01 strict capable user agent.
html :: Html h => h -> h
html = unicodeString "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\
                     \ \"http://www.w3.org/TR/html4/strict.dtd\">\n"
       `mappend` nodeElement (unicodeString "html")

testDoc :: Html h => h
testDoc = html $ head ! attribute "onload" testAtt $ htmlContent testString

testDocUtf8 = runHtmlUtf8 testDoc
testDocLatin1 = runHtmlLatin1 testDoc

urlFragment :: UnicodeSequence s => Unescaped s -> s
urlFragment = escapeURL

cssFragment :: UnicodeSequence s => Unescaped s -> s
cssFragment = escapeCSS

href :: Html h => Unescaped h -> Attribute h
href = attribute "href" . replaceUnencodable urlCharReference
-}
