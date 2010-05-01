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

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

import Text.Blaze (Html, parent, leaf)

-- | Combinator for the @<a>@ element.
--
-- Example:
--
-- > a $ text "foo"
--
-- Result:
--
-- > <a>foo</a>
--
a :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
a =
    let begin, end :: ByteString
        begin = "<a"
        end = "</a>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE a#-}

-- | Combinator for the @<abbr>@ element.
--
-- Example:
--
-- > abbr $ text "foo"
--
-- Result:
--
-- > <abbr>foo</abbr>
--
abbr :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
abbr =
    let begin, end :: ByteString
        begin = "<abbr"
        end = "</abbr>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE abbr#-}

-- | Combinator for the @<acronym>@ element.
--
-- Example:
--
-- > acronym $ text "foo"
--
-- Result:
--
-- > <acronym>foo</acronym>
--
acronym :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
acronym =
    let begin, end :: ByteString
        begin = "<acronym"
        end = "</acronym>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE acronym#-}

-- | Combinator for the @<address>@ element.
--
-- Example:
--
-- > address $ text "foo"
--
-- Result:
--
-- > <address>foo</address>
--
address :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
address =
    let begin, end :: ByteString
        begin = "<address"
        end = "</address>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE address#-}

-- | Combinator for the @<area />@ element.
--
-- Example:
--
-- > area
--
-- Result:
--
-- > <area />
--
area :: Html -- ^ Resulting HTML.
area =
    let begin :: ByteString
        begin = "<area"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE area#-}

-- | Combinator for the @<b>@ element.
--
-- Example:
--
-- > b $ text "foo"
--
-- Result:
--
-- > <b>foo</b>
--
b :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
b =
    let begin, end :: ByteString
        begin = "<b"
        end = "</b>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE b#-}

-- | Combinator for the @<bdo>@ element.
--
-- Example:
--
-- > bdo $ text "foo"
--
-- Result:
--
-- > <bdo>foo</bdo>
--
bdo :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
bdo =
    let begin, end :: ByteString
        begin = "<bdo"
        end = "</bdo>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE bdo#-}

-- | Combinator for the @<big>@ element.
--
-- Example:
--
-- > big $ text "foo"
--
-- Result:
--
-- > <big>foo</big>
--
big :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
big =
    let begin, end :: ByteString
        begin = "<big"
        end = "</big>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE big#-}

-- | Combinator for the @<blockquote>@ element.
--
-- Example:
--
-- > blockquote $ text "foo"
--
-- Result:
--
-- > <blockquote>foo</blockquote>
--
blockquote :: Html -- ^ Inner HTML.
           -> Html -- ^ Resulting HTML.
blockquote =
    let begin, end :: ByteString
        begin = "<blockquote"
        end = "</blockquote>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE blockquote#-}

-- | Combinator for the @<body>@ element.
--
-- Example:
--
-- > body $ text "foo"
--
-- Result:
--
-- > <body>foo</body>
--
body :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
body =
    let begin, end :: ByteString
        begin = "<body"
        end = "</body>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE body#-}

-- | Combinator for the @<br />@ element.
--
-- Example:
--
-- > br
--
-- Result:
--
-- > <br />
--
br :: Html -- ^ Resulting HTML.
br =
    let begin :: ByteString
        begin = "<br"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE br#-}

-- | Combinator for the @<button>@ element.
--
-- Example:
--
-- > button $ text "foo"
--
-- Result:
--
-- > <button>foo</button>
--
button :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
button =
    let begin, end :: ByteString
        begin = "<button"
        end = "</button>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE button#-}

-- | Combinator for the @<caption>@ element.
--
-- Example:
--
-- > caption $ text "foo"
--
-- Result:
--
-- > <caption>foo</caption>
--
caption :: Html -- ^ Inner HTML.
        -> Html -- ^ Resulting HTML.
caption =
    let begin, end :: ByteString
        begin = "<caption"
        end = "</caption>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE caption#-}

-- | Combinator for the @<cite>@ element.
--
-- Example:
--
-- > cite $ text "foo"
--
-- Result:
--
-- > <cite>foo</cite>
--
cite :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
cite =
    let begin, end :: ByteString
        begin = "<cite"
        end = "</cite>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE cite#-}

-- | Combinator for the @<code>@ element.
--
-- Example:
--
-- > code $ text "foo"
--
-- Result:
--
-- > <code>foo</code>
--
code :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
code =
    let begin, end :: ByteString
        begin = "<code"
        end = "</code>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE code#-}

-- | Combinator for the @<col />@ element.
--
-- Example:
--
-- > col
--
-- Result:
--
-- > <col />
--
col :: Html -- ^ Resulting HTML.
col =
    let begin :: ByteString
        begin = "<col"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE col#-}

-- | Combinator for the @<colgroup>@ element.
--
-- Example:
--
-- > colgroup $ text "foo"
--
-- Result:
--
-- > <colgroup>foo</colgroup>
--
colgroup :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
colgroup =
    let begin, end :: ByteString
        begin = "<colgroup"
        end = "</colgroup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE colgroup#-}

-- | Combinator for the @<dd>@ element.
--
-- Example:
--
-- > dd $ text "foo"
--
-- Result:
--
-- > <dd>foo</dd>
--
dd :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dd =
    let begin, end :: ByteString
        begin = "<dd"
        end = "</dd>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE dd#-}

-- | Combinator for the @<del>@ element.
--
-- Example:
--
-- > del $ text "foo"
--
-- Result:
--
-- > <del>foo</del>
--
del :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
del =
    let begin, end :: ByteString
        begin = "<del"
        end = "</del>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE del#-}

-- | Combinator for the @<dfn>@ element.
--
-- Example:
--
-- > dfn $ text "foo"
--
-- Result:
--
-- > <dfn>foo</dfn>
--
dfn :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
dfn =
    let begin, end :: ByteString
        begin = "<dfn"
        end = "</dfn>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE dfn#-}

-- | Combinator for the @<div>@ element.
--
-- Example:
--
-- > div $ text "foo"
--
-- Result:
--
-- > <div>foo</div>
--
div :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
div =
    let begin, end :: ByteString
        begin = "<div"
        end = "</div>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE div#-}

-- | Combinator for the @<dl>@ element.
--
-- Example:
--
-- > dl $ text "foo"
--
-- Result:
--
-- > <dl>foo</dl>
--
dl :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dl =
    let begin, end :: ByteString
        begin = "<dl"
        end = "</dl>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE dl#-}

-- | Combinator for the @<dt>@ element.
--
-- Example:
--
-- > dt $ text "foo"
--
-- Result:
--
-- > <dt>foo</dt>
--
dt :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
dt =
    let begin, end :: ByteString
        begin = "<dt"
        end = "</dt>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE dt#-}

-- | Combinator for the @<em>@ element.
--
-- Example:
--
-- > em $ text "foo"
--
-- Result:
--
-- > <em>foo</em>
--
em :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
em =
    let begin, end :: ByteString
        begin = "<em"
        end = "</em>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE em#-}

-- | Combinator for the @<fieldset>@ element.
--
-- Example:
--
-- > fieldset $ text "foo"
--
-- Result:
--
-- > <fieldset>foo</fieldset>
--
fieldset :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
fieldset =
    let begin, end :: ByteString
        begin = "<fieldset"
        end = "</fieldset>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE fieldset#-}

-- | Combinator for the @<form>@ element.
--
-- Example:
--
-- > form $ text "foo"
--
-- Result:
--
-- > <form>foo</form>
--
form :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
form =
    let begin, end :: ByteString
        begin = "<form"
        end = "</form>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE form#-}

-- | Combinator for the @<h1>@ element.
--
-- Example:
--
-- > h1 $ text "foo"
--
-- Result:
--
-- > <h1>foo</h1>
--
h1 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h1 =
    let begin, end :: ByteString
        begin = "<h1"
        end = "</h1>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h1#-}

-- | Combinator for the @<h2>@ element.
--
-- Example:
--
-- > h2 $ text "foo"
--
-- Result:
--
-- > <h2>foo</h2>
--
h2 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h2 =
    let begin, end :: ByteString
        begin = "<h2"
        end = "</h2>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h2#-}

-- | Combinator for the @<h3>@ element.
--
-- Example:
--
-- > h3 $ text "foo"
--
-- Result:
--
-- > <h3>foo</h3>
--
h3 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h3 =
    let begin, end :: ByteString
        begin = "<h3"
        end = "</h3>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h3#-}

-- | Combinator for the @<h4>@ element.
--
-- Example:
--
-- > h4 $ text "foo"
--
-- Result:
--
-- > <h4>foo</h4>
--
h4 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h4 =
    let begin, end :: ByteString
        begin = "<h4"
        end = "</h4>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h4#-}

-- | Combinator for the @<h5>@ element.
--
-- Example:
--
-- > h5 $ text "foo"
--
-- Result:
--
-- > <h5>foo</h5>
--
h5 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h5 =
    let begin, end :: ByteString
        begin = "<h5"
        end = "</h5>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h5#-}

-- | Combinator for the @<h6>@ element.
--
-- Example:
--
-- > h6 $ text "foo"
--
-- Result:
--
-- > <h6>foo</h6>
--
h6 :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
h6 =
    let begin, end :: ByteString
        begin = "<h6"
        end = "</h6>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE h6#-}

-- | Combinator for the @<head>@ element.
--
-- Example:
--
-- > head $ text "foo"
--
-- Result:
--
-- > <head>foo</head>
--
head :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
head =
    let begin, end :: ByteString
        begin = "<head"
        end = "</head>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE head#-}

-- | Combinator for the @<hr />@ element.
--
-- Example:
--
-- > hr
--
-- Result:
--
-- > <hr />
--
hr :: Html -- ^ Resulting HTML.
hr =
    let begin :: ByteString
        begin = "<hr"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE hr#-}

-- | Combinator for the @<html>@ element.
--
-- Example:
--
-- > html $ text "foo"
--
-- Result:
--
-- > <html>foo</html>
--
html :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
html =
    let begin, end :: ByteString
        begin = "<html"
        end = "</html>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE html#-}

-- | Combinator for the @<i>@ element.
--
-- Example:
--
-- > i $ text "foo"
--
-- Result:
--
-- > <i>foo</i>
--
i :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
i =
    let begin, end :: ByteString
        begin = "<i"
        end = "</i>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE i#-}

-- | Combinator for the @<img />@ element.
--
-- Example:
--
-- > img
--
-- Result:
--
-- > <img />
--
img :: Html -- ^ Resulting HTML.
img =
    let begin :: ByteString
        begin = "<img"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE img#-}

-- | Combinator for the @<input />@ element.
--
-- Example:
--
-- > input
--
-- Result:
--
-- > <input />
--
input :: Html -- ^ Resulting HTML.
input =
    let begin :: ByteString
        begin = "<input"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE input#-}

-- | Combinator for the @<ins>@ element.
--
-- Example:
--
-- > ins $ text "foo"
--
-- Result:
--
-- > <ins>foo</ins>
--
ins :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
ins =
    let begin, end :: ByteString
        begin = "<ins"
        end = "</ins>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE ins#-}

-- | Combinator for the @<kbd>@ element.
--
-- Example:
--
-- > kbd $ text "foo"
--
-- Result:
--
-- > <kbd>foo</kbd>
--
kbd :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
kbd =
    let begin, end :: ByteString
        begin = "<kbd"
        end = "</kbd>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE kbd#-}

-- | Combinator for the @<label>@ element.
--
-- Example:
--
-- > label $ text "foo"
--
-- Result:
--
-- > <label>foo</label>
--
label :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
label =
    let begin, end :: ByteString
        begin = "<label"
        end = "</label>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE label#-}

-- | Combinator for the @<legend>@ element.
--
-- Example:
--
-- > legend $ text "foo"
--
-- Result:
--
-- > <legend>foo</legend>
--
legend :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
legend =
    let begin, end :: ByteString
        begin = "<legend"
        end = "</legend>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE legend#-}

-- | Combinator for the @<li>@ element.
--
-- Example:
--
-- > li $ text "foo"
--
-- Result:
--
-- > <li>foo</li>
--
li :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
li =
    let begin, end :: ByteString
        begin = "<li"
        end = "</li>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE li#-}

-- | Combinator for the @<link />@ element.
--
-- Example:
--
-- > link
--
-- Result:
--
-- > <link />
--
link :: Html -- ^ Resulting HTML.
link =
    let begin :: ByteString
        begin = "<link"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE link#-}

-- | Combinator for the @<map>@ element.
--
-- Example:
--
-- > map $ text "foo"
--
-- Result:
--
-- > <map>foo</map>
--
map :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
map =
    let begin, end :: ByteString
        begin = "<map"
        end = "</map>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE map#-}

-- | Combinator for the @<meta />@ element.
--
-- Example:
--
-- > meta
--
-- Result:
--
-- > <meta />
--
meta :: Html -- ^ Resulting HTML.
meta =
    let begin :: ByteString
        begin = "<meta"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE meta#-}

-- | Combinator for the @<noscript>@ element.
--
-- Example:
--
-- > noscript $ text "foo"
--
-- Result:
--
-- > <noscript>foo</noscript>
--
noscript :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
noscript =
    let begin, end :: ByteString
        begin = "<noscript"
        end = "</noscript>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE noscript#-}

-- | Combinator for the @<object>@ element.
--
-- Example:
--
-- > object $ text "foo"
--
-- Result:
--
-- > <object>foo</object>
--
object :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
object =
    let begin, end :: ByteString
        begin = "<object"
        end = "</object>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE object#-}

-- | Combinator for the @<ol>@ element.
--
-- Example:
--
-- > ol $ text "foo"
--
-- Result:
--
-- > <ol>foo</ol>
--
ol :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
ol =
    let begin, end :: ByteString
        begin = "<ol"
        end = "</ol>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE ol#-}

-- | Combinator for the @<optgroup>@ element.
--
-- Example:
--
-- > optgroup $ text "foo"
--
-- Result:
--
-- > <optgroup>foo</optgroup>
--
optgroup :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
optgroup =
    let begin, end :: ByteString
        begin = "<optgroup"
        end = "</optgroup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE optgroup#-}

-- | Combinator for the @<option>@ element.
--
-- Example:
--
-- > option $ text "foo"
--
-- Result:
--
-- > <option>foo</option>
--
option :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
option =
    let begin, end :: ByteString
        begin = "<option"
        end = "</option>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE option#-}

-- | Combinator for the @<p>@ element.
--
-- Example:
--
-- > p $ text "foo"
--
-- Result:
--
-- > <p>foo</p>
--
p :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
p =
    let begin, end :: ByteString
        begin = "<p"
        end = "</p>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE p#-}

-- | Combinator for the @<param />@ element.
--
-- Example:
--
-- > param
--
-- Result:
--
-- > <param />
--
param :: Html -- ^ Resulting HTML.
param =
    let begin :: ByteString
        begin = "<param"
        {-# NOINLINE begin #-}
    in leaf begin
{-# INLINE param#-}

-- | Combinator for the @<pre>@ element.
--
-- Example:
--
-- > pre $ text "foo"
--
-- Result:
--
-- > <pre>foo</pre>
--
pre :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
pre =
    let begin, end :: ByteString
        begin = "<pre"
        end = "</pre>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE pre#-}

-- | Combinator for the @<q>@ element.
--
-- Example:
--
-- > q $ text "foo"
--
-- Result:
--
-- > <q>foo</q>
--
q :: Html -- ^ Inner HTML.
  -> Html -- ^ Resulting HTML.
q =
    let begin, end :: ByteString
        begin = "<q"
        end = "</q>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE q#-}

-- | Combinator for the @<samp>@ element.
--
-- Example:
--
-- > samp $ text "foo"
--
-- Result:
--
-- > <samp>foo</samp>
--
samp :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
samp =
    let begin, end :: ByteString
        begin = "<samp"
        end = "</samp>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE samp#-}

-- | Combinator for the @<script>@ element.
--
-- Example:
--
-- > script $ text "foo"
--
-- Result:
--
-- > <script>foo</script>
--
script :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
script =
    let begin, end :: ByteString
        begin = "<script"
        end = "</script>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE script#-}

-- | Combinator for the @<select>@ element.
--
-- Example:
--
-- > select $ text "foo"
--
-- Result:
--
-- > <select>foo</select>
--
select :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
select =
    let begin, end :: ByteString
        begin = "<select"
        end = "</select>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE select#-}

-- | Combinator for the @<small>@ element.
--
-- Example:
--
-- > small $ text "foo"
--
-- Result:
--
-- > <small>foo</small>
--
small :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
small =
    let begin, end :: ByteString
        begin = "<small"
        end = "</small>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE small#-}

-- | Combinator for the @<span>@ element.
--
-- Example:
--
-- > span $ text "foo"
--
-- Result:
--
-- > <span>foo</span>
--
span :: Html -- ^ Inner HTML.
     -> Html -- ^ Resulting HTML.
span =
    let begin, end :: ByteString
        begin = "<span"
        end = "</span>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE span#-}

-- | Combinator for the @<strong>@ element.
--
-- Example:
--
-- > strong $ text "foo"
--
-- Result:
--
-- > <strong>foo</strong>
--
strong :: Html -- ^ Inner HTML.
       -> Html -- ^ Resulting HTML.
strong =
    let begin, end :: ByteString
        begin = "<strong"
        end = "</strong>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE strong#-}

-- | Combinator for the @<style>@ element.
--
-- Example:
--
-- > style $ text "foo"
--
-- Result:
--
-- > <style>foo</style>
--
style :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
style =
    let begin, end :: ByteString
        begin = "<style"
        end = "</style>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE style#-}

-- | Combinator for the @<sub>@ element.
--
-- Example:
--
-- > sub $ text "foo"
--
-- Result:
--
-- > <sub>foo</sub>
--
sub :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
sub =
    let begin, end :: ByteString
        begin = "<sub"
        end = "</sub>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE sub#-}

-- | Combinator for the @<sup>@ element.
--
-- Example:
--
-- > sup $ text "foo"
--
-- Result:
--
-- > <sup>foo</sup>
--
sup :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
sup =
    let begin, end :: ByteString
        begin = "<sup"
        end = "</sup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE sup#-}

-- | Combinator for the @<table>@ element.
--
-- Example:
--
-- > table $ text "foo"
--
-- Result:
--
-- > <table>foo</table>
--
table :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
table =
    let begin, end :: ByteString
        begin = "<table"
        end = "</table>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE table#-}

-- | Combinator for the @<tbody>@ element.
--
-- Example:
--
-- > tbody $ text "foo"
--
-- Result:
--
-- > <tbody>foo</tbody>
--
tbody :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
tbody =
    let begin, end :: ByteString
        begin = "<tbody"
        end = "</tbody>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE tbody#-}

-- | Combinator for the @<td>@ element.
--
-- Example:
--
-- > td $ text "foo"
--
-- Result:
--
-- > <td>foo</td>
--
td :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
td =
    let begin, end :: ByteString
        begin = "<td"
        end = "</td>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE td#-}

-- | Combinator for the @<textarea>@ element.
--
-- Example:
--
-- > textarea $ text "foo"
--
-- Result:
--
-- > <textarea>foo</textarea>
--
textarea :: Html -- ^ Inner HTML.
         -> Html -- ^ Resulting HTML.
textarea =
    let begin, end :: ByteString
        begin = "<textarea"
        end = "</textarea>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE textarea#-}

-- | Combinator for the @<tfoot>@ element.
--
-- Example:
--
-- > tfoot $ text "foo"
--
-- Result:
--
-- > <tfoot>foo</tfoot>
--
tfoot :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
tfoot =
    let begin, end :: ByteString
        begin = "<tfoot"
        end = "</tfoot>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE tfoot#-}

-- | Combinator for the @<th>@ element.
--
-- Example:
--
-- > th $ text "foo"
--
-- Result:
--
-- > <th>foo</th>
--
th :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
th =
    let begin, end :: ByteString
        begin = "<th"
        end = "</th>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE th#-}

-- | Combinator for the @<thead>@ element.
--
-- Example:
--
-- > thead $ text "foo"
--
-- Result:
--
-- > <thead>foo</thead>
--
thead :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
thead =
    let begin, end :: ByteString
        begin = "<thead"
        end = "</thead>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE thead#-}

-- | Combinator for the @<title>@ element.
--
-- Example:
--
-- > title $ text "foo"
--
-- Result:
--
-- > <title>foo</title>
--
title :: Html -- ^ Inner HTML.
      -> Html -- ^ Resulting HTML.
title =
    let begin, end :: ByteString
        begin = "<title"
        end = "</title>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE title#-}

-- | Combinator for the @<tr>@ element.
--
-- Example:
--
-- > tr $ text "foo"
--
-- Result:
--
-- > <tr>foo</tr>
--
tr :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
tr =
    let begin, end :: ByteString
        begin = "<tr"
        end = "</tr>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE tr#-}

-- | Combinator for the @<tt>@ element.
--
-- Example:
--
-- > tt $ text "foo"
--
-- Result:
--
-- > <tt>foo</tt>
--
tt :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
tt =
    let begin, end :: ByteString
        begin = "<tt"
        end = "</tt>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE tt#-}

-- | Combinator for the @<ul>@ element.
--
-- Example:
--
-- > ul $ text "foo"
--
-- Result:
--
-- > <ul>foo</ul>
--
ul :: Html -- ^ Inner HTML.
   -> Html -- ^ Resulting HTML.
ul =
    let begin, end :: ByteString
        begin = "<ul"
        end = "</ul>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE ul#-}

-- | Combinator for the @<var>@ element.
--
-- Example:
--
-- > var $ text "foo"
--
-- Result:
--
-- > <var>foo</var>
--
var :: Html -- ^ Inner HTML.
    -> Html -- ^ Resulting HTML.
var =
    let begin, end :: ByteString
        begin = "<var"
        end = "</var>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE var#-}
