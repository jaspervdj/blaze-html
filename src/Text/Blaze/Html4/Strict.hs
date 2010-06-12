{-# LANGUAGE OverloadedStrings #-}
-- | This module exports HTML combinators used to create documents.
--
module Text.Blaze.Html4.Strict
    ( module Text.Blaze
    , html
    , docType
    , a
    , abbr
    , acronym
    , address
    , area
    , b
    , bdo
    , big
    , blockquote
    , body
    , br
    , button
    , caption
    , cite
    , code
    , col
    , colgroup
    , dd
    , del
    , dfn
    , div
    , dl
    , dt
    , em
    , fieldset
    , form
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , head
    , hr
    , htmlNoDocType
    , i
    , img
    , input
    , ins
    , kbd
    , label
    , legend
    , li
    , link
    , map
    , meta
    , noscript
    , object
    , ol
    , optgroup
    , option
    , p
    , param
    , pre
    , q
    , samp
    , script
    , select
    , small
    , span
    , strong
    , style
    , sub
    , sup
    , table
    , tbody
    , td
    , textarea
    , tfoot
    , th
    , thead
    , title
    , tr
    , tt
    , ul
    , var
    ) where

import Prelude ()
import Data.Monoid (mappend)

import Text.Blaze

-- | Combinator for the @\<html>@ element. This combinator will also
-- insert the correct doctype.
--
-- Example:
--
-- > html $ span $ text "foo"
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
-- >     "http://www.w3.org/TR/html4/strict.dtd">
-- > <html><span>foo</span></html>
--
html :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
html inner = docType `mappend` htmlNoDocType inner
{-# INLINE html #-}

-- | Combinator for the document type. This should be placed at the top
-- of every HTML page.
--
-- Example:
--
-- > docType
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
-- >     "http://www.w3.org/TR/html4/strict.dtd">
--
docType :: Html a  -- ^ The document type HTML.
docType = preEscapedText "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n    \"http://www.w3.org/TR/html4/strict.dtd\">\n"
{-# INLINE docType #-}

-- | Combinator for the @\<a>@ element.
--
-- Example:
--
-- > a $ span $ text "foo"
--
-- Result:
--
-- > <a><span>foo</span></a>
--
a :: Html a  -- ^ Inner HTML.
  -> Html b  -- ^ Resulting HTML.
a = parent "<a" "</a>"
{-# INLINE a #-}

-- | Combinator for the @\<abbr>@ element.
--
-- Example:
--
-- > abbr $ span $ text "foo"
--
-- Result:
--
-- > <abbr><span>foo</span></abbr>
--
abbr :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
abbr = parent "<abbr" "</abbr>"
{-# INLINE abbr #-}

-- | Combinator for the @\<acronym>@ element.
--
-- Example:
--
-- > acronym $ span $ text "foo"
--
-- Result:
--
-- > <acronym><span>foo</span></acronym>
--
acronym :: Html a  -- ^ Inner HTML.
        -> Html b  -- ^ Resulting HTML.
acronym = parent "<acronym" "</acronym>"
{-# INLINE acronym #-}

-- | Combinator for the @\<address>@ element.
--
-- Example:
--
-- > address $ span $ text "foo"
--
-- Result:
--
-- > <address><span>foo</span></address>
--
address :: Html a  -- ^ Inner HTML.
        -> Html b  -- ^ Resulting HTML.
address = parent "<address" "</address>"
{-# INLINE address #-}

-- | Combinator for the @\<area>@ element.
--
-- Example:
--
-- > area
--
-- Result:
--
-- > <area>
--
area :: Html a  -- ^ Resulting HTML.
area = open "<area"
{-# INLINE area #-}

-- | Combinator for the @\<b>@ element.
--
-- Example:
--
-- > b $ span $ text "foo"
--
-- Result:
--
-- > <b><span>foo</span></b>
--
b :: Html a  -- ^ Inner HTML.
  -> Html b  -- ^ Resulting HTML.
b = parent "<b" "</b>"
{-# INLINE b #-}

-- | Combinator for the @\<bdo>@ element.
--
-- Example:
--
-- > bdo $ span $ text "foo"
--
-- Result:
--
-- > <bdo><span>foo</span></bdo>
--
bdo :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
bdo = parent "<bdo" "</bdo>"
{-# INLINE bdo #-}

-- | Combinator for the @\<big>@ element.
--
-- Example:
--
-- > big $ span $ text "foo"
--
-- Result:
--
-- > <big><span>foo</span></big>
--
big :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
big = parent "<big" "</big>"
{-# INLINE big #-}

-- | Combinator for the @\<blockquote>@ element.
--
-- Example:
--
-- > blockquote $ span $ text "foo"
--
-- Result:
--
-- > <blockquote><span>foo</span></blockquote>
--
blockquote :: Html a  -- ^ Inner HTML.
           -> Html b  -- ^ Resulting HTML.
blockquote = parent "<blockquote" "</blockquote>"
{-# INLINE blockquote #-}

-- | Combinator for the @\<body>@ element.
--
-- Example:
--
-- > body $ span $ text "foo"
--
-- Result:
--
-- > <body><span>foo</span></body>
--
body :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
body = parent "<body" "</body>"
{-# INLINE body #-}

-- | Combinator for the @\<br>@ element.
--
-- Example:
--
-- > br
--
-- Result:
--
-- > <br>
--
br :: Html a  -- ^ Resulting HTML.
br = open "<br"
{-# INLINE br #-}

-- | Combinator for the @\<button>@ element.
--
-- Example:
--
-- > button $ span $ text "foo"
--
-- Result:
--
-- > <button><span>foo</span></button>
--
button :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
button = parent "<button" "</button>"
{-# INLINE button #-}

-- | Combinator for the @\<caption>@ element.
--
-- Example:
--
-- > caption $ span $ text "foo"
--
-- Result:
--
-- > <caption><span>foo</span></caption>
--
caption :: Html a  -- ^ Inner HTML.
        -> Html b  -- ^ Resulting HTML.
caption = parent "<caption" "</caption>"
{-# INLINE caption #-}

-- | Combinator for the @\<cite>@ element.
--
-- Example:
--
-- > cite $ span $ text "foo"
--
-- Result:
--
-- > <cite><span>foo</span></cite>
--
cite :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
cite = parent "<cite" "</cite>"
{-# INLINE cite #-}

-- | Combinator for the @\<code>@ element.
--
-- Example:
--
-- > code $ span $ text "foo"
--
-- Result:
--
-- > <code><span>foo</span></code>
--
code :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
code = parent "<code" "</code>"
{-# INLINE code #-}

-- | Combinator for the @\<col>@ element.
--
-- Example:
--
-- > col
--
-- Result:
--
-- > <col>
--
col :: Html a  -- ^ Resulting HTML.
col = open "<col"
{-# INLINE col #-}

-- | Combinator for the @\<colgroup>@ element.
--
-- Example:
--
-- > colgroup $ span $ text "foo"
--
-- Result:
--
-- > <colgroup><span>foo</span></colgroup>
--
colgroup :: Html a  -- ^ Inner HTML.
         -> Html b  -- ^ Resulting HTML.
colgroup = parent "<colgroup" "</colgroup>"
{-# INLINE colgroup #-}

-- | Combinator for the @\<dd>@ element.
--
-- Example:
--
-- > dd $ span $ text "foo"
--
-- Result:
--
-- > <dd><span>foo</span></dd>
--
dd :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
dd = parent "<dd" "</dd>"
{-# INLINE dd #-}

-- | Combinator for the @\<del>@ element.
--
-- Example:
--
-- > del $ span $ text "foo"
--
-- Result:
--
-- > <del><span>foo</span></del>
--
del :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
del = parent "<del" "</del>"
{-# INLINE del #-}

-- | Combinator for the @\<dfn>@ element.
--
-- Example:
--
-- > dfn $ span $ text "foo"
--
-- Result:
--
-- > <dfn><span>foo</span></dfn>
--
dfn :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
dfn = parent "<dfn" "</dfn>"
{-# INLINE dfn #-}

-- | Combinator for the @\<div>@ element.
--
-- Example:
--
-- > div $ span $ text "foo"
--
-- Result:
--
-- > <div><span>foo</span></div>
--
div :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
div = parent "<div" "</div>"
{-# INLINE div #-}

-- | Combinator for the @\<dl>@ element.
--
-- Example:
--
-- > dl $ span $ text "foo"
--
-- Result:
--
-- > <dl><span>foo</span></dl>
--
dl :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
dl = parent "<dl" "</dl>"
{-# INLINE dl #-}

-- | Combinator for the @\<dt>@ element.
--
-- Example:
--
-- > dt $ span $ text "foo"
--
-- Result:
--
-- > <dt><span>foo</span></dt>
--
dt :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
dt = parent "<dt" "</dt>"
{-# INLINE dt #-}

-- | Combinator for the @\<em>@ element.
--
-- Example:
--
-- > em $ span $ text "foo"
--
-- Result:
--
-- > <em><span>foo</span></em>
--
em :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
em = parent "<em" "</em>"
{-# INLINE em #-}

-- | Combinator for the @\<fieldset>@ element.
--
-- Example:
--
-- > fieldset $ span $ text "foo"
--
-- Result:
--
-- > <fieldset><span>foo</span></fieldset>
--
fieldset :: Html a  -- ^ Inner HTML.
         -> Html b  -- ^ Resulting HTML.
fieldset = parent "<fieldset" "</fieldset>"
{-# INLINE fieldset #-}

-- | Combinator for the @\<form>@ element.
--
-- Example:
--
-- > form $ span $ text "foo"
--
-- Result:
--
-- > <form><span>foo</span></form>
--
form :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
form = parent "<form" "</form>"
{-# INLINE form #-}

-- | Combinator for the @\<h1>@ element.
--
-- Example:
--
-- > h1 $ span $ text "foo"
--
-- Result:
--
-- > <h1><span>foo</span></h1>
--
h1 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h1 = parent "<h1" "</h1>"
{-# INLINE h1 #-}

-- | Combinator for the @\<h2>@ element.
--
-- Example:
--
-- > h2 $ span $ text "foo"
--
-- Result:
--
-- > <h2><span>foo</span></h2>
--
h2 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h2 = parent "<h2" "</h2>"
{-# INLINE h2 #-}

-- | Combinator for the @\<h3>@ element.
--
-- Example:
--
-- > h3 $ span $ text "foo"
--
-- Result:
--
-- > <h3><span>foo</span></h3>
--
h3 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h3 = parent "<h3" "</h3>"
{-# INLINE h3 #-}

-- | Combinator for the @\<h4>@ element.
--
-- Example:
--
-- > h4 $ span $ text "foo"
--
-- Result:
--
-- > <h4><span>foo</span></h4>
--
h4 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h4 = parent "<h4" "</h4>"
{-# INLINE h4 #-}

-- | Combinator for the @\<h5>@ element.
--
-- Example:
--
-- > h5 $ span $ text "foo"
--
-- Result:
--
-- > <h5><span>foo</span></h5>
--
h5 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h5 = parent "<h5" "</h5>"
{-# INLINE h5 #-}

-- | Combinator for the @\<h6>@ element.
--
-- Example:
--
-- > h6 $ span $ text "foo"
--
-- Result:
--
-- > <h6><span>foo</span></h6>
--
h6 :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
h6 = parent "<h6" "</h6>"
{-# INLINE h6 #-}

-- | Combinator for the @\<head>@ element.
--
-- Example:
--
-- > head $ span $ text "foo"
--
-- Result:
--
-- > <head><span>foo</span></head>
--
head :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
head = parent "<head" "</head>"
{-# INLINE head #-}

-- | Combinator for the @\<hr>@ element.
--
-- Example:
--
-- > hr
--
-- Result:
--
-- > <hr>
--
hr :: Html a  -- ^ Resulting HTML.
hr = open "<hr"
{-# INLINE hr #-}

-- | Combinator for the @\<html>@ element.
--
-- Example:
--
-- > htmlNoDocType $ span $ text "foo"
--
-- Result:
--
-- > <html><span>foo</span></html>
--
htmlNoDocType :: Html a  -- ^ Inner HTML.
              -> Html b  -- ^ Resulting HTML.
htmlNoDocType = parent "<html" "</html>"
{-# INLINE htmlNoDocType #-}

-- | Combinator for the @\<i>@ element.
--
-- Example:
--
-- > i $ span $ text "foo"
--
-- Result:
--
-- > <i><span>foo</span></i>
--
i :: Html a  -- ^ Inner HTML.
  -> Html b  -- ^ Resulting HTML.
i = parent "<i" "</i>"
{-# INLINE i #-}

-- | Combinator for the @\<img>@ element.
--
-- Example:
--
-- > img
--
-- Result:
--
-- > <img>
--
img :: Html a  -- ^ Resulting HTML.
img = open "<img"
{-# INLINE img #-}

-- | Combinator for the @\<input>@ element.
--
-- Example:
--
-- > input
--
-- Result:
--
-- > <input>
--
input :: Html a  -- ^ Resulting HTML.
input = open "<input"
{-# INLINE input #-}

-- | Combinator for the @\<ins>@ element.
--
-- Example:
--
-- > ins $ span $ text "foo"
--
-- Result:
--
-- > <ins><span>foo</span></ins>
--
ins :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
ins = parent "<ins" "</ins>"
{-# INLINE ins #-}

-- | Combinator for the @\<kbd>@ element.
--
-- Example:
--
-- > kbd $ span $ text "foo"
--
-- Result:
--
-- > <kbd><span>foo</span></kbd>
--
kbd :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
kbd = parent "<kbd" "</kbd>"
{-# INLINE kbd #-}

-- | Combinator for the @\<label>@ element.
--
-- Example:
--
-- > label $ span $ text "foo"
--
-- Result:
--
-- > <label><span>foo</span></label>
--
label :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
label = parent "<label" "</label>"
{-# INLINE label #-}

-- | Combinator for the @\<legend>@ element.
--
-- Example:
--
-- > legend $ span $ text "foo"
--
-- Result:
--
-- > <legend><span>foo</span></legend>
--
legend :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
legend = parent "<legend" "</legend>"
{-# INLINE legend #-}

-- | Combinator for the @\<li>@ element.
--
-- Example:
--
-- > li $ span $ text "foo"
--
-- Result:
--
-- > <li><span>foo</span></li>
--
li :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
li = parent "<li" "</li>"
{-# INLINE li #-}

-- | Combinator for the @\<link>@ element.
--
-- Example:
--
-- > link
--
-- Result:
--
-- > <link>
--
link :: Html a  -- ^ Resulting HTML.
link = open "<link"
{-# INLINE link #-}

-- | Combinator for the @\<map>@ element.
--
-- Example:
--
-- > map $ span $ text "foo"
--
-- Result:
--
-- > <map><span>foo</span></map>
--
map :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
map = parent "<map" "</map>"
{-# INLINE map #-}

-- | Combinator for the @\<meta>@ element.
--
-- Example:
--
-- > meta
--
-- Result:
--
-- > <meta>
--
meta :: Html a  -- ^ Resulting HTML.
meta = open "<meta"
{-# INLINE meta #-}

-- | Combinator for the @\<noscript>@ element.
--
-- Example:
--
-- > noscript $ span $ text "foo"
--
-- Result:
--
-- > <noscript><span>foo</span></noscript>
--
noscript :: Html a  -- ^ Inner HTML.
         -> Html b  -- ^ Resulting HTML.
noscript = parent "<noscript" "</noscript>"
{-# INLINE noscript #-}

-- | Combinator for the @\<object>@ element.
--
-- Example:
--
-- > object $ span $ text "foo"
--
-- Result:
--
-- > <object><span>foo</span></object>
--
object :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
object = parent "<object" "</object>"
{-# INLINE object #-}

-- | Combinator for the @\<ol>@ element.
--
-- Example:
--
-- > ol $ span $ text "foo"
--
-- Result:
--
-- > <ol><span>foo</span></ol>
--
ol :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
ol = parent "<ol" "</ol>"
{-# INLINE ol #-}

-- | Combinator for the @\<optgroup>@ element.
--
-- Example:
--
-- > optgroup $ span $ text "foo"
--
-- Result:
--
-- > <optgroup><span>foo</span></optgroup>
--
optgroup :: Html a  -- ^ Inner HTML.
         -> Html b  -- ^ Resulting HTML.
optgroup = parent "<optgroup" "</optgroup>"
{-# INLINE optgroup #-}

-- | Combinator for the @\<option>@ element.
--
-- Example:
--
-- > option $ span $ text "foo"
--
-- Result:
--
-- > <option><span>foo</span></option>
--
option :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
option = parent "<option" "</option>"
{-# INLINE option #-}

-- | Combinator for the @\<p>@ element.
--
-- Example:
--
-- > p $ span $ text "foo"
--
-- Result:
--
-- > <p><span>foo</span></p>
--
p :: Html a  -- ^ Inner HTML.
  -> Html b  -- ^ Resulting HTML.
p = parent "<p" "</p>"
{-# INLINE p #-}

-- | Combinator for the @\<param>@ element.
--
-- Example:
--
-- > param
--
-- Result:
--
-- > <param>
--
param :: Html a  -- ^ Resulting HTML.
param = open "<param"
{-# INLINE param #-}

-- | Combinator for the @\<pre>@ element.
--
-- Example:
--
-- > pre $ span $ text "foo"
--
-- Result:
--
-- > <pre><span>foo</span></pre>
--
pre :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
pre = parent "<pre" "</pre>"
{-# INLINE pre #-}

-- | Combinator for the @\<q>@ element.
--
-- Example:
--
-- > q $ span $ text "foo"
--
-- Result:
--
-- > <q><span>foo</span></q>
--
q :: Html a  -- ^ Inner HTML.
  -> Html b  -- ^ Resulting HTML.
q = parent "<q" "</q>"
{-# INLINE q #-}

-- | Combinator for the @\<samp>@ element.
--
-- Example:
--
-- > samp $ span $ text "foo"
--
-- Result:
--
-- > <samp><span>foo</span></samp>
--
samp :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
samp = parent "<samp" "</samp>"
{-# INLINE samp #-}

-- | Combinator for the @\<script>@ element.
--
-- Example:
--
-- > script $ span $ text "foo"
--
-- Result:
--
-- > <script><span>foo</span></script>
--
script :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
script = parent "<script" "</script>"
{-# INLINE script #-}

-- | Combinator for the @\<select>@ element.
--
-- Example:
--
-- > select $ span $ text "foo"
--
-- Result:
--
-- > <select><span>foo</span></select>
--
select :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
select = parent "<select" "</select>"
{-# INLINE select #-}

-- | Combinator for the @\<small>@ element.
--
-- Example:
--
-- > small $ span $ text "foo"
--
-- Result:
--
-- > <small><span>foo</span></small>
--
small :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
small = parent "<small" "</small>"
{-# INLINE small #-}

-- | Combinator for the @\<span>@ element.
--
-- Example:
--
-- > span $ span $ text "foo"
--
-- Result:
--
-- > <span><span>foo</span></span>
--
span :: Html a  -- ^ Inner HTML.
     -> Html b  -- ^ Resulting HTML.
span = parent "<span" "</span>"
{-# INLINE span #-}

-- | Combinator for the @\<strong>@ element.
--
-- Example:
--
-- > strong $ span $ text "foo"
--
-- Result:
--
-- > <strong><span>foo</span></strong>
--
strong :: Html a  -- ^ Inner HTML.
       -> Html b  -- ^ Resulting HTML.
strong = parent "<strong" "</strong>"
{-# INLINE strong #-}

-- | Combinator for the @\<style>@ element.
--
-- Example:
--
-- > style $ span $ text "foo"
--
-- Result:
--
-- > <style><span>foo</span></style>
--
style :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
style = parent "<style" "</style>"
{-# INLINE style #-}

-- | Combinator for the @\<sub>@ element.
--
-- Example:
--
-- > sub $ span $ text "foo"
--
-- Result:
--
-- > <sub><span>foo</span></sub>
--
sub :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
sub = parent "<sub" "</sub>"
{-# INLINE sub #-}

-- | Combinator for the @\<sup>@ element.
--
-- Example:
--
-- > sup $ span $ text "foo"
--
-- Result:
--
-- > <sup><span>foo</span></sup>
--
sup :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
sup = parent "<sup" "</sup>"
{-# INLINE sup #-}

-- | Combinator for the @\<table>@ element.
--
-- Example:
--
-- > table $ span $ text "foo"
--
-- Result:
--
-- > <table><span>foo</span></table>
--
table :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
table = parent "<table" "</table>"
{-# INLINE table #-}

-- | Combinator for the @\<tbody>@ element.
--
-- Example:
--
-- > tbody $ span $ text "foo"
--
-- Result:
--
-- > <tbody><span>foo</span></tbody>
--
tbody :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
tbody = parent "<tbody" "</tbody>"
{-# INLINE tbody #-}

-- | Combinator for the @\<td>@ element.
--
-- Example:
--
-- > td $ span $ text "foo"
--
-- Result:
--
-- > <td><span>foo</span></td>
--
td :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
td = parent "<td" "</td>"
{-# INLINE td #-}

-- | Combinator for the @\<textarea>@ element.
--
-- Example:
--
-- > textarea $ span $ text "foo"
--
-- Result:
--
-- > <textarea><span>foo</span></textarea>
--
textarea :: Html a  -- ^ Inner HTML.
         -> Html b  -- ^ Resulting HTML.
textarea = parent "<textarea" "</textarea>"
{-# INLINE textarea #-}

-- | Combinator for the @\<tfoot>@ element.
--
-- Example:
--
-- > tfoot $ span $ text "foo"
--
-- Result:
--
-- > <tfoot><span>foo</span></tfoot>
--
tfoot :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
tfoot = parent "<tfoot" "</tfoot>"
{-# INLINE tfoot #-}

-- | Combinator for the @\<th>@ element.
--
-- Example:
--
-- > th $ span $ text "foo"
--
-- Result:
--
-- > <th><span>foo</span></th>
--
th :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
th = parent "<th" "</th>"
{-# INLINE th #-}

-- | Combinator for the @\<thead>@ element.
--
-- Example:
--
-- > thead $ span $ text "foo"
--
-- Result:
--
-- > <thead><span>foo</span></thead>
--
thead :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
thead = parent "<thead" "</thead>"
{-# INLINE thead #-}

-- | Combinator for the @\<title>@ element.
--
-- Example:
--
-- > title $ span $ text "foo"
--
-- Result:
--
-- > <title><span>foo</span></title>
--
title :: Html a  -- ^ Inner HTML.
      -> Html b  -- ^ Resulting HTML.
title = parent "<title" "</title>"
{-# INLINE title #-}

-- | Combinator for the @\<tr>@ element.
--
-- Example:
--
-- > tr $ span $ text "foo"
--
-- Result:
--
-- > <tr><span>foo</span></tr>
--
tr :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
tr = parent "<tr" "</tr>"
{-# INLINE tr #-}

-- | Combinator for the @\<tt>@ element.
--
-- Example:
--
-- > tt $ span $ text "foo"
--
-- Result:
--
-- > <tt><span>foo</span></tt>
--
tt :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
tt = parent "<tt" "</tt>"
{-# INLINE tt #-}

-- | Combinator for the @\<ul>@ element.
--
-- Example:
--
-- > ul $ span $ text "foo"
--
-- Result:
--
-- > <ul><span>foo</span></ul>
--
ul :: Html a  -- ^ Inner HTML.
   -> Html b  -- ^ Resulting HTML.
ul = parent "<ul" "</ul>"
{-# INLINE ul #-}

-- | Combinator for the @\<var>@ element.
--
-- Example:
--
-- > var $ span $ text "foo"
--
-- Result:
--
-- > <var><span>foo</span></var>
--
var :: Html a  -- ^ Inner HTML.
    -> Html b  -- ^ Resulting HTML.
var = parent "<var" "</var>"
{-# INLINE var #-}
