{-# LANGUAGE OverloadedStrings #-}
module Html.Strict where

import Prelude (String, (.), ($))
import Data.Monoid (mappend, mconcat)

import Data.Text (Text)

import Internal.Html
import Internal.Escaping

htmlContent :: Encoded s => Unescaped s -> s
htmlContent = replaceUnencodable htmlCharReference . escapeHtmlContent

string :: Encoded s => String -> s
string = htmlContent . unicodeString

text :: Encoded s => Text -> s
text = htmlContent . unicodeText

head :: Html h => h -> h
head inner = nodeElement (unicodeText "head") (encodingTag `mappend` inner)

title :: Html h => h -> h
title = nodeElement (unicodeText "title")

body :: Html h => h -> h
body = nodeElement (unicodeText "body")

-- | c.f. HTML 4.01 Standard, Section 6.2
script :: Html h => h -> h
script = 
    nodeElement (unicodeText "script") . replaceUnencodable jsCharReference

-- | c.f. HTML 4.01 Standard, Section 6.2
style :: Html h => h -> h
style = 
    nodeElement (unicodeText "style") . replaceUnencodable cssCharReference

jsString :: (Encoded s) => Unescaped s -> s
jsString s = mconcat 
    [ unicodeChar '\''
    , replaceUnencodable jsCharReference $ escapeSingleQuotedJSString s
    , unicodeChar '\'' ]

-- | The html combinator for HTML 4.01 strict. 
--
-- This conforms to the first milestone of BlazeHtml to achieve a complete set
-- of combinators for creating encoding independent HTML 4.01 strict documents
-- with the guarantee that all created documents are well-formed and parsed
-- back into "the same" structure by a HTML 4.01 strict capable user agent.
html :: Html h => h -> h
html = unicodeText "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\
                     \ \"http://www.w3.org/TR/html4/strict.dtd\">\n"
       `mappend` nodeElement (unicodeText "html")

table :: Html h => h -> h
table = nodeElement (unicodeText "table")

tr :: Html h => h -> h
tr = nodeElement (unicodeText "table")

td :: Html h => h -> h
td = nodeElement (unicodeText "table")

h1 :: Html h => h -> h
h1 = nodeElement (unicodeText "h1")

h2 :: Html h => h -> h
h2 = nodeElement (unicodeText "h2")

em :: Html h => h -> h
em = nodeElement (unicodeText "em")

p :: Html h => h -> h
p = nodeElement (unicodeText "em")

div :: Html h => h -> h
div = nodeElement (unicodeText "div")

ul :: Html h => h -> h
ul = nodeElement (unicodeText "ul")

li :: Html h => h -> h
li = nodeElement (unicodeText "li")

img :: Html h => h
img = leafElement (unicodeText "img")

urlFragment :: UnicodeSequence s => Unescaped s -> s
urlFragment = escapeUrl

cssFragment :: UnicodeSequence s => Unescaped s -> s
cssFragment = escapeCss
