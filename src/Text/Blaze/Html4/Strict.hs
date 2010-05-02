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
a =
    let begin, end :: ByteString
        begin = "<a"
        end = "</a>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
abbr =
    let begin, end :: ByteString
        begin = "<abbr"
        end = "</abbr>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
acronym =
    let begin, end :: ByteString
        begin = "<acronym"
        end = "</acronym>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
address =
    let begin, end :: ByteString
        begin = "<address"
        end = "</address>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE address #-}

-- | Combinator for the @\<area />@ element.
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
b =
    let begin, end :: ByteString
        begin = "<b"
        end = "</b>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
bdo =
    let begin, end :: ByteString
        begin = "<bdo"
        end = "</bdo>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
big =
    let begin, end :: ByteString
        begin = "<big"
        end = "</big>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
blockquote =
    let begin, end :: ByteString
        begin = "<blockquote"
        end = "</blockquote>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
body =
    let begin, end :: ByteString
        begin = "<body"
        end = "</body>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE body #-}

-- | Combinator for the @\<br />@ element.
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
button =
    let begin, end :: ByteString
        begin = "<button"
        end = "</button>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
caption =
    let begin, end :: ByteString
        begin = "<caption"
        end = "</caption>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
cite =
    let begin, end :: ByteString
        begin = "<cite"
        end = "</cite>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
code =
    let begin, end :: ByteString
        begin = "<code"
        end = "</code>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE code #-}

-- | Combinator for the @\<col />@ element.
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
colgroup =
    let begin, end :: ByteString
        begin = "<colgroup"
        end = "</colgroup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
dd =
    let begin, end :: ByteString
        begin = "<dd"
        end = "</dd>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
del =
    let begin, end :: ByteString
        begin = "<del"
        end = "</del>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
dfn =
    let begin, end :: ByteString
        begin = "<dfn"
        end = "</dfn>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
div =
    let begin, end :: ByteString
        begin = "<div"
        end = "</div>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
dl =
    let begin, end :: ByteString
        begin = "<dl"
        end = "</dl>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
dt =
    let begin, end :: ByteString
        begin = "<dt"
        end = "</dt>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
em =
    let begin, end :: ByteString
        begin = "<em"
        end = "</em>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
fieldset =
    let begin, end :: ByteString
        begin = "<fieldset"
        end = "</fieldset>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
form =
    let begin, end :: ByteString
        begin = "<form"
        end = "</form>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h1 =
    let begin, end :: ByteString
        begin = "<h1"
        end = "</h1>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h2 =
    let begin, end :: ByteString
        begin = "<h2"
        end = "</h2>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h3 =
    let begin, end :: ByteString
        begin = "<h3"
        end = "</h3>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h4 =
    let begin, end :: ByteString
        begin = "<h4"
        end = "</h4>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h5 =
    let begin, end :: ByteString
        begin = "<h5"
        end = "</h5>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
h6 =
    let begin, end :: ByteString
        begin = "<h6"
        end = "</h6>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
head =
    let begin, end :: ByteString
        begin = "<head"
        end = "</head>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE head #-}

-- | Combinator for the @\<hr />@ element.
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
html =
    let begin, end :: ByteString
        begin = "<html"
        end = "</html>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
i =
    let begin, end :: ByteString
        begin = "<i"
        end = "</i>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE i #-}

-- | Combinator for the @\<img />@ element.
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
{-# INLINE img #-}

-- | Combinator for the @\<input />@ element.
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
ins =
    let begin, end :: ByteString
        begin = "<ins"
        end = "</ins>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
kbd =
    let begin, end :: ByteString
        begin = "<kbd"
        end = "</kbd>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
label =
    let begin, end :: ByteString
        begin = "<label"
        end = "</label>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
legend =
    let begin, end :: ByteString
        begin = "<legend"
        end = "</legend>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
li =
    let begin, end :: ByteString
        begin = "<li"
        end = "</li>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE li #-}

-- | Combinator for the @\<link />@ element.
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
map =
    let begin, end :: ByteString
        begin = "<map"
        end = "</map>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE map #-}

-- | Combinator for the @\<meta />@ element.
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
noscript =
    let begin, end :: ByteString
        begin = "<noscript"
        end = "</noscript>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
object =
    let begin, end :: ByteString
        begin = "<object"
        end = "</object>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
ol =
    let begin, end :: ByteString
        begin = "<ol"
        end = "</ol>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
optgroup =
    let begin, end :: ByteString
        begin = "<optgroup"
        end = "</optgroup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
option =
    let begin, end :: ByteString
        begin = "<option"
        end = "</option>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
p =
    let begin, end :: ByteString
        begin = "<p"
        end = "</p>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE p #-}

-- | Combinator for the @\<param />@ element.
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
pre =
    let begin, end :: ByteString
        begin = "<pre"
        end = "</pre>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
q =
    let begin, end :: ByteString
        begin = "<q"
        end = "</q>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
samp =
    let begin, end :: ByteString
        begin = "<samp"
        end = "</samp>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
script =
    let begin, end :: ByteString
        begin = "<script"
        end = "</script>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
select =
    let begin, end :: ByteString
        begin = "<select"
        end = "</select>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
small =
    let begin, end :: ByteString
        begin = "<small"
        end = "</small>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
span =
    let begin, end :: ByteString
        begin = "<span"
        end = "</span>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
strong =
    let begin, end :: ByteString
        begin = "<strong"
        end = "</strong>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
style =
    let begin, end :: ByteString
        begin = "<style"
        end = "</style>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
sub =
    let begin, end :: ByteString
        begin = "<sub"
        end = "</sub>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
sup =
    let begin, end :: ByteString
        begin = "<sup"
        end = "</sup>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
table =
    let begin, end :: ByteString
        begin = "<table"
        end = "</table>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
tbody =
    let begin, end :: ByteString
        begin = "<tbody"
        end = "</tbody>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
td =
    let begin, end :: ByteString
        begin = "<td"
        end = "</td>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
textarea =
    let begin, end :: ByteString
        begin = "<textarea"
        end = "</textarea>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
tfoot =
    let begin, end :: ByteString
        begin = "<tfoot"
        end = "</tfoot>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
th =
    let begin, end :: ByteString
        begin = "<th"
        end = "</th>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
thead =
    let begin, end :: ByteString
        begin = "<thead"
        end = "</thead>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
title =
    let begin, end :: ByteString
        begin = "<title"
        end = "</title>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
tr =
    let begin, end :: ByteString
        begin = "<tr"
        end = "</tr>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
tt =
    let begin, end :: ByteString
        begin = "<tt"
        end = "</tt>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
ul =
    let begin, end :: ByteString
        begin = "<ul"
        end = "</ul>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
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
var =
    let begin, end :: ByteString
        begin = "<var"
        end = "</var>"
        {-# NOINLINE begin #-}
        {-# NOINLINE end #-}
    in parent begin end
{-# INLINE var #-}
