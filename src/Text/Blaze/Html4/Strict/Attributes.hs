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
import Data.ByteString.Char8 (ByteString)

import Text.Blaze (Attribute, attribute)

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
abbr :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
abbr =
    let begin :: ByteString
        begin = " abbr=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
accept :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
accept =
    let begin :: ByteString
        begin = " accept=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
accesskey :: Text      -- ^ Attribute value.
          -> Attribute -- ^ Resulting attribute.
accesskey =
    let begin :: ByteString
        begin = " accesskey=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
action :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
action =
    let begin :: ByteString
        begin = " action=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
align :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
align =
    let begin :: ByteString
        begin = " align=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
alt :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
alt =
    let begin :: ByteString
        begin = " alt=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
archive :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
archive =
    let begin :: ByteString
        begin = " archive=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
axis :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
axis =
    let begin :: ByteString
        begin = " axis=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
border :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
border =
    let begin :: ByteString
        begin = " border=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
cellpadding :: Text      -- ^ Attribute value.
            -> Attribute -- ^ Resulting attribute.
cellpadding =
    let begin :: ByteString
        begin = " cellpadding=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
cellspacing :: Text      -- ^ Attribute value.
            -> Attribute -- ^ Resulting attribute.
cellspacing =
    let begin :: ByteString
        begin = " cellspacing=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
char :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
char =
    let begin :: ByteString
        begin = " char=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
charoff :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
charoff =
    let begin :: ByteString
        begin = " charoff=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
charset :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
charset =
    let begin :: ByteString
        begin = " charset=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
checked :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
checked =
    let begin :: ByteString
        begin = " checked=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
cite :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
cite =
    let begin :: ByteString
        begin = " cite=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
class_ :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
class_ =
    let begin :: ByteString
        begin = " class=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
classid :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
classid =
    let begin :: ByteString
        begin = " classid=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
codebase :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
codebase =
    let begin :: ByteString
        begin = " codebase=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
codetype :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
codetype =
    let begin :: ByteString
        begin = " codetype=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
cols :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
cols =
    let begin :: ByteString
        begin = " cols=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
colspan :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
colspan =
    let begin :: ByteString
        begin = " colspan=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
content :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
content =
    let begin :: ByteString
        begin = " content=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
coords :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
coords =
    let begin :: ByteString
        begin = " coords=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
data_ :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
data_ =
    let begin :: ByteString
        begin = " data=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
datetime :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
datetime =
    let begin :: ByteString
        begin = " datetime=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
declare :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
declare =
    let begin :: ByteString
        begin = " declare=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
defer :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
defer =
    let begin :: ByteString
        begin = " defer=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
dir :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
dir =
    let begin :: ByteString
        begin = " dir=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
disabled :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
disabled =
    let begin :: ByteString
        begin = " disabled=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
for :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
for =
    let begin :: ByteString
        begin = " for=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
frame :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
frame =
    let begin :: ByteString
        begin = " frame=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
headers :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
headers =
    let begin :: ByteString
        begin = " headers=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
height :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
height =
    let begin :: ByteString
        begin = " height=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
href :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
href =
    let begin :: ByteString
        begin = " href=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
hreflang :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
hreflang =
    let begin :: ByteString
        begin = " hreflang=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
http_equiv :: Text      -- ^ Attribute value.
           -> Attribute -- ^ Resulting attribute.
http_equiv =
    let begin :: ByteString
        begin = " http-equiv=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
id :: Text      -- ^ Attribute value.
   -> Attribute -- ^ Resulting attribute.
id =
    let begin :: ByteString
        begin = " id=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
label :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
label =
    let begin :: ByteString
        begin = " label=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
lang :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
lang =
    let begin :: ByteString
        begin = " lang=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
maxlength :: Text      -- ^ Attribute value.
          -> Attribute -- ^ Resulting attribute.
maxlength =
    let begin :: ByteString
        begin = " maxlength=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
media :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
media =
    let begin :: ByteString
        begin = " media=\""
        {-# NOINLINE begin #-}
    in attribute begin
{-# INLINE media #-}

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
multiple :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
multiple =
    let begin :: ByteString
        begin = " multiple=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
name :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
name =
    let begin :: ByteString
        begin = " name=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
nohref :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
nohref =
    let begin :: ByteString
        begin = " nohref=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onabort :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
onabort =
    let begin :: ByteString
        begin = " onabort=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onblur :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
onblur =
    let begin :: ByteString
        begin = " onblur=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onchange :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
onchange =
    let begin :: ByteString
        begin = " onchange=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onclick :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
onclick =
    let begin :: ByteString
        begin = " onclick=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
ondblclick :: Text      -- ^ Attribute value.
           -> Attribute -- ^ Resulting attribute.
ondblclick =
    let begin :: ByteString
        begin = " ondblclick=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onfocus :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
onfocus =
    let begin :: ByteString
        begin = " onfocus=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onkeydown :: Text      -- ^ Attribute value.
          -> Attribute -- ^ Resulting attribute.
onkeydown =
    let begin :: ByteString
        begin = " onkeydown=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onkeypress :: Text      -- ^ Attribute value.
           -> Attribute -- ^ Resulting attribute.
onkeypress =
    let begin :: ByteString
        begin = " onkeypress=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onkeyup :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
onkeyup =
    let begin :: ByteString
        begin = " onkeyup=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onload :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
onload =
    let begin :: ByteString
        begin = " onload=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onmousedown :: Text      -- ^ Attribute value.
            -> Attribute -- ^ Resulting attribute.
onmousedown =
    let begin :: ByteString
        begin = " onmousedown=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onmousemove :: Text      -- ^ Attribute value.
            -> Attribute -- ^ Resulting attribute.
onmousemove =
    let begin :: ByteString
        begin = " onmousemove=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onmouseout :: Text      -- ^ Attribute value.
           -> Attribute -- ^ Resulting attribute.
onmouseout =
    let begin :: ByteString
        begin = " onmouseout=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onmouseover :: Text      -- ^ Attribute value.
            -> Attribute -- ^ Resulting attribute.
onmouseover =
    let begin :: ByteString
        begin = " onmouseover=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onmouseup :: Text      -- ^ Attribute value.
          -> Attribute -- ^ Resulting attribute.
onmouseup =
    let begin :: ByteString
        begin = " onmouseup=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onreset :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
onreset =
    let begin :: ByteString
        begin = " onreset=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onselect :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
onselect =
    let begin :: ByteString
        begin = " onselect=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onsubmit :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
onsubmit =
    let begin :: ByteString
        begin = " onsubmit=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
onunload :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
onunload =
    let begin :: ByteString
        begin = " onunload=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
profile :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
profile =
    let begin :: ByteString
        begin = " profile=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
readonly :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
readonly =
    let begin :: ByteString
        begin = " readonly=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
rel :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
rel =
    let begin :: ByteString
        begin = " rel=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
rev :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
rev =
    let begin :: ByteString
        begin = " rev=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
rows :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
rows =
    let begin :: ByteString
        begin = " rows=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
rowspan :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
rowspan =
    let begin :: ByteString
        begin = " rowspan=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
rules :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
rules =
    let begin :: ByteString
        begin = " rules=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
scheme :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
scheme =
    let begin :: ByteString
        begin = " scheme=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
scope :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
scope =
    let begin :: ByteString
        begin = " scope=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
selected :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
selected =
    let begin :: ByteString
        begin = " selected=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
shape :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
shape =
    let begin :: ByteString
        begin = " shape=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
size :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
size =
    let begin :: ByteString
        begin = " size=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
span :: Text      -- ^ Attribute value.
     -> Attribute -- ^ Resulting attribute.
span =
    let begin :: ByteString
        begin = " span=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
src :: Text      -- ^ Attribute value.
    -> Attribute -- ^ Resulting attribute.
src =
    let begin :: ByteString
        begin = " src=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
standby :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
standby =
    let begin :: ByteString
        begin = " standby=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
style :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
style =
    let begin :: ByteString
        begin = " style=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
summary :: Text      -- ^ Attribute value.
        -> Attribute -- ^ Resulting attribute.
summary =
    let begin :: ByteString
        begin = " summary=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
tabindex :: Text      -- ^ Attribute value.
         -> Attribute -- ^ Resulting attribute.
tabindex =
    let begin :: ByteString
        begin = " tabindex=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
title :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
title =
    let begin :: ByteString
        begin = " title=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
type_ :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
type_ =
    let begin :: ByteString
        begin = " type=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
usemap :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
usemap =
    let begin :: ByteString
        begin = " usemap=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
valign :: Text      -- ^ Attribute value.
       -> Attribute -- ^ Resulting attribute.
valign =
    let begin :: ByteString
        begin = " valign=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
value :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
value =
    let begin :: ByteString
        begin = " value=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
valuetype :: Text      -- ^ Attribute value.
          -> Attribute -- ^ Resulting attribute.
valuetype =
    let begin :: ByteString
        begin = " valuetype=\""
        {-# NOINLINE begin #-}
    in attribute begin
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
width :: Text      -- ^ Attribute value.
      -> Attribute -- ^ Resulting attribute.
width =
    let begin :: ByteString
        begin = " width=\""
        {-# NOINLINE begin #-}
    in attribute begin
{-# INLINE width #-}
