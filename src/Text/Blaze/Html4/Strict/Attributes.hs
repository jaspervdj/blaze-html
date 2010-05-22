{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html4.Strict.Attributes
    ( abbr
    , accept
    , accesskey
    , action
    , align
    , alt
    , archive
    , axis
    , border
    , cellpadding
    , cellspacing
    , char
    , charoff
    , charset
    , checked
    , cite
    , class_
    , classid
    , codebase
    , codetype
    , cols
    , colspan
    , content
    , coords
    , data_
    , datetime
    , declare
    , defer
    , dir
    , disabled
    , for
    , frame
    , headers
    , height
    , href
    , hreflang
    , http_equiv
    , id
    , label
    , lang
    , maxlength
    , media
    , method
    , multiple
    , name
    , nohref
    , onabort
    , onblur
    , onchange
    , onclick
    , ondblclick
    , onfocus
    , onkeydown
    , onkeypress
    , onkeyup
    , onload
    , onmousedown
    , onmousemove
    , onmouseout
    , onmouseover
    , onmouseup
    , onreset
    , onselect
    , onsubmit
    , onunload
    , profile
    , readonly
    , rel
    , rev
    , rows
    , rowspan
    , rules
    , scheme
    , scope
    , selected
    , shape
    , size
    , span
    , src
    , standby
    , style
    , summary
    , tabindex
    , title
    , type_
    , usemap
    , valign
    , value
    , valuetype
    , width
    ) where

import Prelude ()

import Data.Text (Text)

import Text.Blaze (Attribute, AttributeValue, attribute)

-- | Combinator for the @abbr@ attribute.
--
-- Example:
--
-- > img ! abbr "bar"
--
-- Result:
--
-- > <img abbr="bar" />
--
abbr :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
abbr = attribute "abbr"
{-# INLINE abbr #-}

-- | Combinator for the @accept@ attribute.
--
-- Example:
--
-- > img ! accept "bar"
--
-- Result:
--
-- > <img accept="bar" />
--
accept :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
accept = attribute "accept"
{-# INLINE accept #-}

-- | Combinator for the @accesskey@ attribute.
--
-- Example:
--
-- > img ! accesskey "bar"
--
-- Result:
--
-- > <img accesskey="bar" />
--
accesskey :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
accesskey = attribute "accesskey"
{-# INLINE accesskey #-}

-- | Combinator for the @action@ attribute.
--
-- Example:
--
-- > img ! action "bar"
--
-- Result:
--
-- > <img action="bar" />
--
action :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
action = attribute "action"
{-# INLINE action #-}

-- | Combinator for the @align@ attribute.
--
-- Example:
--
-- > img ! align "bar"
--
-- Result:
--
-- > <img align="bar" />
--
align :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
align = attribute "align"
{-# INLINE align #-}

-- | Combinator for the @alt@ attribute.
--
-- Example:
--
-- > img ! alt "bar"
--
-- Result:
--
-- > <img alt="bar" />
--
alt :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
alt = attribute "alt"
{-# INLINE alt #-}

-- | Combinator for the @archive@ attribute.
--
-- Example:
--
-- > img ! archive "bar"
--
-- Result:
--
-- > <img archive="bar" />
--
archive :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
archive = attribute "archive"
{-# INLINE archive #-}

-- | Combinator for the @axis@ attribute.
--
-- Example:
--
-- > img ! axis "bar"
--
-- Result:
--
-- > <img axis="bar" />
--
axis :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
axis = attribute "axis"
{-# INLINE axis #-}

-- | Combinator for the @border@ attribute.
--
-- Example:
--
-- > img ! border "bar"
--
-- Result:
--
-- > <img border="bar" />
--
border :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
border = attribute "border"
{-# INLINE border #-}

-- | Combinator for the @cellpadding@ attribute.
--
-- Example:
--
-- > img ! cellpadding "bar"
--
-- Result:
--
-- > <img cellpadding="bar" />
--
cellpadding :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
cellpadding = attribute "cellpadding"
{-# INLINE cellpadding #-}

-- | Combinator for the @cellspacing@ attribute.
--
-- Example:
--
-- > img ! cellspacing "bar"
--
-- Result:
--
-- > <img cellspacing="bar" />
--
cellspacing :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
cellspacing = attribute "cellspacing"
{-# INLINE cellspacing #-}

-- | Combinator for the @char@ attribute.
--
-- Example:
--
-- > img ! char "bar"
--
-- Result:
--
-- > <img char="bar" />
--
char :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
char = attribute "char"
{-# INLINE char #-}

-- | Combinator for the @charoff@ attribute.
--
-- Example:
--
-- > img ! charoff "bar"
--
-- Result:
--
-- > <img charoff="bar" />
--
charoff :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
charoff = attribute "charoff"
{-# INLINE charoff #-}

-- | Combinator for the @charset@ attribute.
--
-- Example:
--
-- > img ! charset "bar"
--
-- Result:
--
-- > <img charset="bar" />
--
charset :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
charset = attribute "charset"
{-# INLINE charset #-}

-- | Combinator for the @checked@ attribute.
--
-- Example:
--
-- > img ! checked "bar"
--
-- Result:
--
-- > <img checked="bar" />
--
checked :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
checked = attribute "checked"
{-# INLINE checked #-}

-- | Combinator for the @cite@ attribute.
--
-- Example:
--
-- > img ! cite "bar"
--
-- Result:
--
-- > <img cite="bar" />
--
cite :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
cite = attribute "cite"
{-# INLINE cite #-}

-- | Combinator for the @class@ attribute.
--
-- Example:
--
-- > img ! class_ "bar"
--
-- Result:
--
-- > <img class="bar" />
--
class_ :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
class_ = attribute "class"
{-# INLINE class_ #-}

-- | Combinator for the @classid@ attribute.
--
-- Example:
--
-- > img ! classid "bar"
--
-- Result:
--
-- > <img classid="bar" />
--
classid :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
classid = attribute "classid"
{-# INLINE classid #-}

-- | Combinator for the @codebase@ attribute.
--
-- Example:
--
-- > img ! codebase "bar"
--
-- Result:
--
-- > <img codebase="bar" />
--
codebase :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
codebase = attribute "codebase"
{-# INLINE codebase #-}

-- | Combinator for the @codetype@ attribute.
--
-- Example:
--
-- > img ! codetype "bar"
--
-- Result:
--
-- > <img codetype="bar" />
--
codetype :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
codetype = attribute "codetype"
{-# INLINE codetype #-}

-- | Combinator for the @cols@ attribute.
--
-- Example:
--
-- > img ! cols "bar"
--
-- Result:
--
-- > <img cols="bar" />
--
cols :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
cols = attribute "cols"
{-# INLINE cols #-}

-- | Combinator for the @colspan@ attribute.
--
-- Example:
--
-- > img ! colspan "bar"
--
-- Result:
--
-- > <img colspan="bar" />
--
colspan :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
colspan = attribute "colspan"
{-# INLINE colspan #-}

-- | Combinator for the @content@ attribute.
--
-- Example:
--
-- > img ! content "bar"
--
-- Result:
--
-- > <img content="bar" />
--
content :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
content = attribute "content"
{-# INLINE content #-}

-- | Combinator for the @coords@ attribute.
--
-- Example:
--
-- > img ! coords "bar"
--
-- Result:
--
-- > <img coords="bar" />
--
coords :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
coords = attribute "coords"
{-# INLINE coords #-}

-- | Combinator for the @data@ attribute.
--
-- Example:
--
-- > img ! data_ "bar"
--
-- Result:
--
-- > <img data="bar" />
--
data_ :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
data_ = attribute "data"
{-# INLINE data_ #-}

-- | Combinator for the @datetime@ attribute.
--
-- Example:
--
-- > img ! datetime "bar"
--
-- Result:
--
-- > <img datetime="bar" />
--
datetime :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
datetime = attribute "datetime"
{-# INLINE datetime #-}

-- | Combinator for the @declare@ attribute.
--
-- Example:
--
-- > img ! declare "bar"
--
-- Result:
--
-- > <img declare="bar" />
--
declare :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
declare = attribute "declare"
{-# INLINE declare #-}

-- | Combinator for the @defer@ attribute.
--
-- Example:
--
-- > img ! defer "bar"
--
-- Result:
--
-- > <img defer="bar" />
--
defer :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
defer = attribute "defer"
{-# INLINE defer #-}

-- | Combinator for the @dir@ attribute.
--
-- Example:
--
-- > img ! dir "bar"
--
-- Result:
--
-- > <img dir="bar" />
--
dir :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
dir = attribute "dir"
{-# INLINE dir #-}

-- | Combinator for the @disabled@ attribute.
--
-- Example:
--
-- > img ! disabled "bar"
--
-- Result:
--
-- > <img disabled="bar" />
--
disabled :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
disabled = attribute "disabled"
{-# INLINE disabled #-}

-- | Combinator for the @for@ attribute.
--
-- Example:
--
-- > img ! for "bar"
--
-- Result:
--
-- > <img for="bar" />
--
for :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
for = attribute "for"
{-# INLINE for #-}

-- | Combinator for the @frame@ attribute.
--
-- Example:
--
-- > img ! frame "bar"
--
-- Result:
--
-- > <img frame="bar" />
--
frame :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
frame = attribute "frame"
{-# INLINE frame #-}

-- | Combinator for the @headers@ attribute.
--
-- Example:
--
-- > img ! headers "bar"
--
-- Result:
--
-- > <img headers="bar" />
--
headers :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
headers = attribute "headers"
{-# INLINE headers #-}

-- | Combinator for the @height@ attribute.
--
-- Example:
--
-- > img ! height "bar"
--
-- Result:
--
-- > <img height="bar" />
--
height :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
height = attribute "height"
{-# INLINE height #-}

-- | Combinator for the @href@ attribute.
--
-- Example:
--
-- > img ! href "bar"
--
-- Result:
--
-- > <img href="bar" />
--
href :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
href = attribute "href"
{-# INLINE href #-}

-- | Combinator for the @hreflang@ attribute.
--
-- Example:
--
-- > img ! hreflang "bar"
--
-- Result:
--
-- > <img hreflang="bar" />
--
hreflang :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
hreflang = attribute "hreflang"
{-# INLINE hreflang #-}

-- | Combinator for the @http-equiv@ attribute.
--
-- Example:
--
-- > img ! http_equiv "bar"
--
-- Result:
--
-- > <img http-equiv="bar" />
--
http_equiv :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
http_equiv = attribute "http-equiv"
{-# INLINE http_equiv #-}

-- | Combinator for the @id@ attribute.
--
-- Example:
--
-- > img ! id "bar"
--
-- Result:
--
-- > <img id="bar" />
--
id :: AttributeValue  -- ^ Attribute value.
   -> Attribute       -- ^ Resulting attribute.
id = attribute "id"
{-# INLINE id #-}

-- | Combinator for the @label@ attribute.
--
-- Example:
--
-- > img ! label "bar"
--
-- Result:
--
-- > <img label="bar" />
--
label :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
label = attribute "label"
{-# INLINE label #-}

-- | Combinator for the @lang@ attribute.
--
-- Example:
--
-- > img ! lang "bar"
--
-- Result:
--
-- > <img lang="bar" />
--
lang :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
lang = attribute "lang"
{-# INLINE lang #-}

-- | Combinator for the @maxlength@ attribute.
--
-- Example:
--
-- > img ! maxlength "bar"
--
-- Result:
--
-- > <img maxlength="bar" />
--
maxlength :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
maxlength = attribute "maxlength"
{-# INLINE maxlength #-}

-- | Combinator for the @media@ attribute.
--
-- Example:
--
-- > img ! media "bar"
--
-- Result:
--
-- > <img media="bar" />
--
media :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
media = attribute "media"
{-# INLINE media #-}

-- | Combinator for the @method@ attribute.
--
-- Example:
--
-- > img ! method "bar"
--
-- Result:
--
-- > <img method="bar" />
--
method :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
method = attribute "method"
{-# INLINE method #-}

-- | Combinator for the @multiple@ attribute.
--
-- Example:
--
-- > img ! multiple "bar"
--
-- Result:
--
-- > <img multiple="bar" />
--
multiple :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
multiple = attribute "multiple"
{-# INLINE multiple #-}

-- | Combinator for the @name@ attribute.
--
-- Example:
--
-- > img ! name "bar"
--
-- Result:
--
-- > <img name="bar" />
--
name :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
name = attribute "name"
{-# INLINE name #-}

-- | Combinator for the @nohref@ attribute.
--
-- Example:
--
-- > img ! nohref "bar"
--
-- Result:
--
-- > <img nohref="bar" />
--
nohref :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
nohref = attribute "nohref"
{-# INLINE nohref #-}

-- | Combinator for the @onabort@ attribute.
--
-- Example:
--
-- > img ! onabort "bar"
--
-- Result:
--
-- > <img onabort="bar" />
--
onabort :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onabort = attribute "onabort"
{-# INLINE onabort #-}

-- | Combinator for the @onblur@ attribute.
--
-- Example:
--
-- > img ! onblur "bar"
--
-- Result:
--
-- > <img onblur="bar" />
--
onblur :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onblur = attribute "onblur"
{-# INLINE onblur #-}

-- | Combinator for the @onchange@ attribute.
--
-- Example:
--
-- > img ! onchange "bar"
--
-- Result:
--
-- > <img onchange="bar" />
--
onchange :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onchange = attribute "onchange"
{-# INLINE onchange #-}

-- | Combinator for the @onclick@ attribute.
--
-- Example:
--
-- > img ! onclick "bar"
--
-- Result:
--
-- > <img onclick="bar" />
--
onclick :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onclick = attribute "onclick"
{-# INLINE onclick #-}

-- | Combinator for the @ondblclick@ attribute.
--
-- Example:
--
-- > img ! ondblclick "bar"
--
-- Result:
--
-- > <img ondblclick="bar" />
--
ondblclick :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
ondblclick = attribute "ondblclick"
{-# INLINE ondblclick #-}

-- | Combinator for the @onfocus@ attribute.
--
-- Example:
--
-- > img ! onfocus "bar"
--
-- Result:
--
-- > <img onfocus="bar" />
--
onfocus :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onfocus = attribute "onfocus"
{-# INLINE onfocus #-}

-- | Combinator for the @onkeydown@ attribute.
--
-- Example:
--
-- > img ! onkeydown "bar"
--
-- Result:
--
-- > <img onkeydown="bar" />
--
onkeydown :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
onkeydown = attribute "onkeydown"
{-# INLINE onkeydown #-}

-- | Combinator for the @onkeypress@ attribute.
--
-- Example:
--
-- > img ! onkeypress "bar"
--
-- Result:
--
-- > <img onkeypress="bar" />
--
onkeypress :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
onkeypress = attribute "onkeypress"
{-# INLINE onkeypress #-}

-- | Combinator for the @onkeyup@ attribute.
--
-- Example:
--
-- > img ! onkeyup "bar"
--
-- Result:
--
-- > <img onkeyup="bar" />
--
onkeyup :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onkeyup = attribute "onkeyup"
{-# INLINE onkeyup #-}

-- | Combinator for the @onload@ attribute.
--
-- Example:
--
-- > img ! onload "bar"
--
-- Result:
--
-- > <img onload="bar" />
--
onload :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onload = attribute "onload"
{-# INLINE onload #-}

-- | Combinator for the @onmousedown@ attribute.
--
-- Example:
--
-- > img ! onmousedown "bar"
--
-- Result:
--
-- > <img onmousedown="bar" />
--
onmousedown :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
onmousedown = attribute "onmousedown"
{-# INLINE onmousedown #-}

-- | Combinator for the @onmousemove@ attribute.
--
-- Example:
--
-- > img ! onmousemove "bar"
--
-- Result:
--
-- > <img onmousemove="bar" />
--
onmousemove :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
onmousemove = attribute "onmousemove"
{-# INLINE onmousemove #-}

-- | Combinator for the @onmouseout@ attribute.
--
-- Example:
--
-- > img ! onmouseout "bar"
--
-- Result:
--
-- > <img onmouseout="bar" />
--
onmouseout :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
onmouseout = attribute "onmouseout"
{-# INLINE onmouseout #-}

-- | Combinator for the @onmouseover@ attribute.
--
-- Example:
--
-- > img ! onmouseover "bar"
--
-- Result:
--
-- > <img onmouseover="bar" />
--
onmouseover :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
onmouseover = attribute "onmouseover"
{-# INLINE onmouseover #-}

-- | Combinator for the @onmouseup@ attribute.
--
-- Example:
--
-- > img ! onmouseup "bar"
--
-- Result:
--
-- > <img onmouseup="bar" />
--
onmouseup :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
onmouseup = attribute "onmouseup"
{-# INLINE onmouseup #-}

-- | Combinator for the @onreset@ attribute.
--
-- Example:
--
-- > img ! onreset "bar"
--
-- Result:
--
-- > <img onreset="bar" />
--
onreset :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onreset = attribute "onreset"
{-# INLINE onreset #-}

-- | Combinator for the @onselect@ attribute.
--
-- Example:
--
-- > img ! onselect "bar"
--
-- Result:
--
-- > <img onselect="bar" />
--
onselect :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onselect = attribute "onselect"
{-# INLINE onselect #-}

-- | Combinator for the @onsubmit@ attribute.
--
-- Example:
--
-- > img ! onsubmit "bar"
--
-- Result:
--
-- > <img onsubmit="bar" />
--
onsubmit :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onsubmit = attribute "onsubmit"
{-# INLINE onsubmit #-}

-- | Combinator for the @onunload@ attribute.
--
-- Example:
--
-- > img ! onunload "bar"
--
-- Result:
--
-- > <img onunload="bar" />
--
onunload :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onunload = attribute "onunload"
{-# INLINE onunload #-}

-- | Combinator for the @profile@ attribute.
--
-- Example:
--
-- > img ! profile "bar"
--
-- Result:
--
-- > <img profile="bar" />
--
profile :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
profile = attribute "profile"
{-# INLINE profile #-}

-- | Combinator for the @readonly@ attribute.
--
-- Example:
--
-- > img ! readonly "bar"
--
-- Result:
--
-- > <img readonly="bar" />
--
readonly :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
readonly = attribute "readonly"
{-# INLINE readonly #-}

-- | Combinator for the @rel@ attribute.
--
-- Example:
--
-- > img ! rel "bar"
--
-- Result:
--
-- > <img rel="bar" />
--
rel :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
rel = attribute "rel"
{-# INLINE rel #-}

-- | Combinator for the @rev@ attribute.
--
-- Example:
--
-- > img ! rev "bar"
--
-- Result:
--
-- > <img rev="bar" />
--
rev :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
rev = attribute "rev"
{-# INLINE rev #-}

-- | Combinator for the @rows@ attribute.
--
-- Example:
--
-- > img ! rows "bar"
--
-- Result:
--
-- > <img rows="bar" />
--
rows :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
rows = attribute "rows"
{-# INLINE rows #-}

-- | Combinator for the @rowspan@ attribute.
--
-- Example:
--
-- > img ! rowspan "bar"
--
-- Result:
--
-- > <img rowspan="bar" />
--
rowspan :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
rowspan = attribute "rowspan"
{-# INLINE rowspan #-}

-- | Combinator for the @rules@ attribute.
--
-- Example:
--
-- > img ! rules "bar"
--
-- Result:
--
-- > <img rules="bar" />
--
rules :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
rules = attribute "rules"
{-# INLINE rules #-}

-- | Combinator for the @scheme@ attribute.
--
-- Example:
--
-- > img ! scheme "bar"
--
-- Result:
--
-- > <img scheme="bar" />
--
scheme :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
scheme = attribute "scheme"
{-# INLINE scheme #-}

-- | Combinator for the @scope@ attribute.
--
-- Example:
--
-- > img ! scope "bar"
--
-- Result:
--
-- > <img scope="bar" />
--
scope :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
scope = attribute "scope"
{-# INLINE scope #-}

-- | Combinator for the @selected@ attribute.
--
-- Example:
--
-- > img ! selected "bar"
--
-- Result:
--
-- > <img selected="bar" />
--
selected :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
selected = attribute "selected"
{-# INLINE selected #-}

-- | Combinator for the @shape@ attribute.
--
-- Example:
--
-- > img ! shape "bar"
--
-- Result:
--
-- > <img shape="bar" />
--
shape :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
shape = attribute "shape"
{-# INLINE shape #-}

-- | Combinator for the @size@ attribute.
--
-- Example:
--
-- > img ! size "bar"
--
-- Result:
--
-- > <img size="bar" />
--
size :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
size = attribute "size"
{-# INLINE size #-}

-- | Combinator for the @span@ attribute.
--
-- Example:
--
-- > img ! span "bar"
--
-- Result:
--
-- > <img span="bar" />
--
span :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
span = attribute "span"
{-# INLINE span #-}

-- | Combinator for the @src@ attribute.
--
-- Example:
--
-- > img ! src "bar"
--
-- Result:
--
-- > <img src="bar" />
--
src :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
src = attribute "src"
{-# INLINE src #-}

-- | Combinator for the @standby@ attribute.
--
-- Example:
--
-- > img ! standby "bar"
--
-- Result:
--
-- > <img standby="bar" />
--
standby :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
standby = attribute "standby"
{-# INLINE standby #-}

-- | Combinator for the @style@ attribute.
--
-- Example:
--
-- > img ! style "bar"
--
-- Result:
--
-- > <img style="bar" />
--
style :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
style = attribute "style"
{-# INLINE style #-}

-- | Combinator for the @summary@ attribute.
--
-- Example:
--
-- > img ! summary "bar"
--
-- Result:
--
-- > <img summary="bar" />
--
summary :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
summary = attribute "summary"
{-# INLINE summary #-}

-- | Combinator for the @tabindex@ attribute.
--
-- Example:
--
-- > img ! tabindex "bar"
--
-- Result:
--
-- > <img tabindex="bar" />
--
tabindex :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
tabindex = attribute "tabindex"
{-# INLINE tabindex #-}

-- | Combinator for the @title@ attribute.
--
-- Example:
--
-- > img ! title "bar"
--
-- Result:
--
-- > <img title="bar" />
--
title :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
title = attribute "title"
{-# INLINE title #-}

-- | Combinator for the @type@ attribute.
--
-- Example:
--
-- > img ! type_ "bar"
--
-- Result:
--
-- > <img type="bar" />
--
type_ :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
type_ = attribute "type"
{-# INLINE type_ #-}

-- | Combinator for the @usemap@ attribute.
--
-- Example:
--
-- > img ! usemap "bar"
--
-- Result:
--
-- > <img usemap="bar" />
--
usemap :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
usemap = attribute "usemap"
{-# INLINE usemap #-}

-- | Combinator for the @valign@ attribute.
--
-- Example:
--
-- > img ! valign "bar"
--
-- Result:
--
-- > <img valign="bar" />
--
valign :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
valign = attribute "valign"
{-# INLINE valign #-}

-- | Combinator for the @value@ attribute.
--
-- Example:
--
-- > img ! value "bar"
--
-- Result:
--
-- > <img value="bar" />
--
value :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
value = attribute "value"
{-# INLINE value #-}

-- | Combinator for the @valuetype@ attribute.
--
-- Example:
--
-- > img ! valuetype "bar"
--
-- Result:
--
-- > <img valuetype="bar" />
--
valuetype :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
valuetype = attribute "valuetype"
{-# INLINE valuetype #-}

-- | Combinator for the @width@ attribute.
--
-- Example:
--
-- > img ! width "bar"
--
-- Result:
--
-- > <img width="bar" />
--
width :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
width = attribute "width"
{-# INLINE width #-}
