{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html4.Strict
    ( a
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
    , html
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

import Text.Blaze (Html, parent, leaf, open)

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
a :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
a = parent "a"
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
abbr :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
abbr = parent "abbr"
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
acronym :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
acronym = parent "acronym"
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
address :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
address = parent "address"
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
area :: Html -- ^ Resulting HTML.
area = open "area"
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
b :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
b = parent "b"
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
bdo :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
bdo = parent "bdo"
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
big :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
big = parent "big"
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
blockquote :: Html -- ^ Inner HTML.
           -> Html -- ^ Resulting HTML.
blockquote = parent "blockquote"
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
body :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
body = parent "body"
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
br :: Html -- ^ Resulting HTML.
br = open "br"
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
button :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
button = parent "button"
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
caption :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
caption = parent "caption"
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
cite :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
cite = parent "cite"
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
code :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
code = parent "code"
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
col :: Html -- ^ Resulting HTML.
col = open "col"
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
colgroup :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
colgroup = parent "colgroup"
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
dd :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dd = parent "dd"
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
del :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
del = parent "del"
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
dfn :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
dfn = parent "dfn"
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
div :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
div = parent "div"
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
dl :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dl = parent "dl"
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
dt :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dt = parent "dt"
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
em :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
em = parent "em"
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
fieldset :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
fieldset = parent "fieldset"
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
form :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
form = parent "form"
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
h1 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h1 = parent "h1"
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
h2 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h2 = parent "h2"
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
h3 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h3 = parent "h3"
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
h4 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h4 = parent "h4"
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
h5 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h5 = parent "h5"
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
h6 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h6 = parent "h6"
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
head :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
head = parent "head"
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
hr :: Html -- ^ Resulting HTML.
hr = open "hr"
{-# INLINE hr #-}

-- | Combinator for the @\<html>@ element.
--
-- Example:
--
-- > html $ span $ text "foo"
--
-- Result:
--
-- > <html><span>foo</span></html>
--
html :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
html = parent "html"
{-# INLINE html #-}

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
i :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
i = parent "i"
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
img :: Html -- ^ Resulting HTML.
img = open "img"
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
input :: Html -- ^ Resulting HTML.
input = open "input"
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
ins :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
ins = parent "ins"
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
kbd :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
kbd = parent "kbd"
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
label :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
label = parent "label"
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
legend :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
legend = parent "legend"
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
li :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
li = parent "li"
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
link :: Html -- ^ Resulting HTML.
link = open "link"
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
map :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
map = parent "map"
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
meta :: Html -- ^ Resulting HTML.
meta = open "meta"
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
noscript :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
noscript = parent "noscript"
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
object :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
object = parent "object"
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
ol :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
ol = parent "ol"
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
optgroup :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
optgroup = parent "optgroup"
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
option :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
option = parent "option"
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
p :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
p = parent "p"
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
param :: Html -- ^ Resulting HTML.
param = open "param"
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
pre :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
pre = parent "pre"
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
q :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
q = parent "q"
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
samp :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
samp = parent "samp"
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
script :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
script = parent "script"
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
select :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
select = parent "select"
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
small :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
small = parent "small"
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
span :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
span = parent "span"
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
strong :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
strong = parent "strong"
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
style :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
style = parent "style"
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
sub :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
sub = parent "sub"
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
sup :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
sup = parent "sup"
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
table :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
table = parent "table"
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
tbody :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
tbody = parent "tbody"
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
td :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
td = parent "td"
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
textarea :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
textarea = parent "textarea"
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
tfoot :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
tfoot = parent "tfoot"
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
th :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
th = parent "th"
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
thead :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
thead = parent "thead"
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
title :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
title = parent "title"
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
tr :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
tr = parent "tr"
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
tt :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
tt = parent "tt"
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
ul :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
ul = parent "ul"
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
var :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
var = parent "var"
{-# INLINE var #-}
