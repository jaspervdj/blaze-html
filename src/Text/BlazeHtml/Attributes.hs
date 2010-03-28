{-# LANGUAGE OverloadedStrings #-}
-- | This module exports common HTML tributes. You probably need to import
-- this under an alias, since a lot of names clash prelude functions. Names
-- clashing with standard Haskell keywords (e.g. @class@) have a @_@ appended.
-- Names including a hyphen have @_@ instead.
--
-- Examples:
--
-- > attribute name         combinator
-- > ---------------------------------
-- > align                  align
-- > div                    div
-- > class                  class_
-- > http-equiv             http_equiv
--
-- Usage example:
--
-- > import Text.BlazeHtml.Attributes
-- > import Text.BlazeHtml.Attributes as A
module Text.BlazeHtml.Attributes
    ( abbr
    , accept_charset
    , accept
    , accesskey
    , action
    , align
    , alink
    , alt
    , archive
    , axis
    , background
    , bgcolor
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
    , clear
    , code
    , codebase
    , codetype
    , color
    , cols
    , colspan
    , compact
    , content
    , coords
    , data_
    , datetime
    , declare
    , defer
    , dir
    , disabled
    , enctype
    , face
    , for
    , frame
    , frameborder
    , headers
    , height
    , href
    , hreflang
    , hspace
    , http_equiv
    , id
    , ismap
    , label
    , lang
    , language
    , link
    , longdesc
    , marginheight
    , marginwidth
    , maxlength
    , media
    , method
    , multiple
    , name
    , nohref
    , noresize
    , noshade
    , nowrap
    , object
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
    , prompt
    , readonly
    , rel
    , rev
    , rows
    , rowspan
    , rules
    , scheme
    , scope
    , scrolling
    , selected
    , shape
    , size
    , span
    , src
    , standby
    , start
    , style
    , summary
    , tabindex
    , target
    , text
    , title
    , type_
    , usemap
    , valign
    , value
    , valuetype
    , version
    , vlink
    , vspace    
    , width
    , xmlns
    ) where

import Prelude ()

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html (Attribute)

-- | Set the @abbr@ attribute.
abbr :: Text -> Attribute
abbr v = ("abbr", v)

-- | Set the @accept_charset@ attribute.
accept_charset :: Text -> Attribute
accept_charset v = ("accept-charset", v)

-- | Set the @accept@ attribute.
accept :: Text -> Attribute
accept v = ("accept", v)

-- | Set the @accesskey@ attribute.
accesskey :: Text -> Attribute
accesskey v = ("accesskey", v)

-- | Set the @action@ attribute.
action :: Text -> Attribute
action v = ("action", v)

-- | Set the @align@ attribute.
align :: Text -> Attribute
align v = ("align", v)

-- | Set the @alink@ attribute.
alink :: Text -> Attribute
alink v = ("alink", v)

-- | Set the @alt@ attribute.
alt :: Text -> Attribute
alt v = ("alt", v)

-- | Set the @archive@ attribute.
archive :: Text -> Attribute
archive v = ("archive", v)

-- | Set the @axis@ attribute.
axis :: Text -> Attribute
axis v = ("axis", v)

-- | Set the @background@ attribute.
background :: Text -> Attribute
background v = ("background", v)

-- | Set the @bgcolor@ attribute.
bgcolor :: Text -> Attribute
bgcolor v = ("bgcolor", v)

-- | Set the @border@ attribute.
border :: Text -> Attribute
border v = ("border", v)

-- | Set the @cellpadding@ attribute.
cellpadding :: Text -> Attribute
cellpadding v = ("cellpadding", v)

-- | Set the @cellspacing@ attribute.
cellspacing :: Text -> Attribute
cellspacing v = ("cellspacing", v)

-- | Set the @char@ attribute.
char :: Text -> Attribute
char v = ("char", v)

-- | Set the @charoff@ attribute.
charoff :: Text -> Attribute
charoff v = ("charoff", v)

-- | Set the @charset@ attribute.
charset :: Text -> Attribute
charset v = ("charset", v)

-- | Set the @checked@ attribute.
checked :: Text -> Attribute
checked v = ("checked", v)

-- | Set the @cite@ attribute.
cite :: Text -> Attribute
cite v = ("cite", v)

-- | Set the @class_@ attribute.
class_ :: Text -> Attribute
class_ v = ("class", v)

-- | Set the @classid@ attribute.
classid :: Text -> Attribute
classid v = ("classid", v)

-- | Set the @clear@ attribute.
clear :: Text -> Attribute
clear v = ("clear", v)

-- | Set the @code@ attribute.
code :: Text -> Attribute
code v = ("code", v)

-- | Set the @codebase@ attribute.
codebase :: Text -> Attribute
codebase v = ("codebase", v)

-- | Set the @codetype@ attribute.
codetype :: Text -> Attribute
codetype v = ("codetype", v)

-- | Set the @color@ attribute.
color :: Text -> Attribute
color v = ("color", v)

-- | Set the @cols@ attribute.
cols :: Text -> Attribute
cols v = ("cols", v)

-- | Set the @colspan@ attribute.
colspan :: Text -> Attribute
colspan v = ("colspan", v)

-- | Set the @compact@ attribute.
compact :: Text -> Attribute
compact v = ("compact", v)

-- | Set the @content@ attribute.
content :: Text -> Attribute
content v = ("content", v)

-- | Set the @coords@ attribute.
coords :: Text -> Attribute
coords v = ("coords", v)

-- | Set the @data_@ attribute.
data_ :: Text -> Attribute
data_ v = ("data", v)

-- | Set the @datetime@ attribute.
datetime :: Text -> Attribute
datetime v = ("datetime", v)

-- | Set the @declare@ attribute.
declare :: Text -> Attribute
declare v = ("declare", v)

-- | Set the @defer@ attribute.
defer :: Text -> Attribute
defer v = ("defer", v)

-- | Set the @dir@ attribute.
dir :: Text -> Attribute
dir v = ("dir", v)

-- | Set the @disabled@ attribute.
disabled :: Text -> Attribute
disabled v = ("disabled", v)

-- | Set the @enctype@ attribute.
enctype :: Text -> Attribute
enctype v = ("enctype", v)

-- | Set the @face@ attribute.
face :: Text -> Attribute
face v = ("face", v)

-- | Set the @for@ attribute.
for :: Text -> Attribute
for v = ("for", v)

-- | Set the @frame@ attribute.
frame :: Text -> Attribute
frame v = ("frame", v)

-- | Set the @frameborder@ attribute.
frameborder :: Text -> Attribute
frameborder v = ("frameborder", v)

-- | Set the @headers@ attribute.
headers :: Text -> Attribute
headers v = ("headers", v)

-- | Set the @height@ attribute.
height :: Text -> Attribute
height v = ("height", v)

-- | Set the @href@ attribute.
href :: Text -> Attribute
href v = ("href", v)

-- | Set the @hreflang@ attribute.
hreflang :: Text -> Attribute
hreflang v = ("hreflang", v)

-- | Set the @hspace@ attribute.
hspace :: Text -> Attribute
hspace v = ("hspace", v)

-- | Set the @http_equiv@ attribute.
http_equiv :: Text -> Attribute
http_equiv v = ("http-equiv", v)

-- | Set the @id@ attribute.
id :: Text -> Attribute
id v = ("id", v)

-- | Set the @ismap@ attribute.
ismap :: Text -> Attribute
ismap v = ("ismap", v)

-- | Set the @label@ attribute.
label :: Text -> Attribute
label v = ("label", v)

-- | Set the @lang@ attribute.
lang :: Text -> Attribute
lang v = ("lang", v)

-- | Set the @language@ attribute.
language :: Text -> Attribute
language v = ("language", v)

-- | Set the @link@ attribute.
link :: Text -> Attribute
link v = ("link", v)

-- | Set the @longdesc@ attribute.
longdesc :: Text -> Attribute
longdesc v = ("longdesc", v)

-- | Set the @marginheight@ attribute.
marginheight :: Text -> Attribute
marginheight v = ("marginheight", v)

-- | Set the @marginwidth@ attribute.
marginwidth :: Text -> Attribute
marginwidth v = ("marginwidth", v)

-- | Set the @maxlength@ attribute.
maxlength :: Text -> Attribute
maxlength v = ("maxlength", v)

-- | Set the @media@ attribute.
media :: Text -> Attribute
media v = ("media", v)

-- | Set the @method@ attribute.
method :: Text -> Attribute
method v = ("method", v)

-- | Set the @multiple@ attribute.
multiple :: Text -> Attribute
multiple v = ("multiple", v)

-- | Set the @name@ attribute.
name :: Text -> Attribute
name v = ("name", v)

-- | Set the @nohref@ attribute.
nohref :: Text -> Attribute
nohref v = ("nohref", v)

-- | Set the @noresize@ attribute.
noresize :: Text -> Attribute
noresize v = ("noresize", v)

-- | Set the @noshade@ attribute.
noshade :: Text -> Attribute
noshade v = ("noshade", v)

-- | Set the @nowrap@ attribute.
nowrap :: Text -> Attribute
nowrap v = ("nowrap", v)

-- | Set the @object@ attribute.
object :: Text -> Attribute
object v = ("object", v)

-- | Set the @onblur@ attribute.
onblur :: Text -> Attribute
onblur v = ("onblur", v)

-- | Set the @onchange@ attribute.
onchange :: Text -> Attribute
onchange v = ("onchange", v)

-- | Set the @onclick@ attribute.
onclick :: Text -> Attribute
onclick v = ("onclick", v)

-- | Set the @ondblclick@ attribute.
ondblclick :: Text -> Attribute
ondblclick v = ("ondblclick", v)

-- | Set the @onfocus@ attribute.
onfocus :: Text -> Attribute
onfocus v = ("onfocus", v)

-- | Set the @onkeydown@ attribute.
onkeydown :: Text -> Attribute
onkeydown v = ("onkeydown", v)

-- | Set the @onkeypress@ attribute.
onkeypress :: Text -> Attribute
onkeypress v = ("onkeypress", v)

-- | Set the @onkeyup@ attribute.
onkeyup :: Text -> Attribute
onkeyup v = ("onkeyup", v)

-- | Set the @onload@ attribute.
onload :: Text -> Attribute
onload v = ("onload", v)

-- | Set the @onmousedown@ attribute.
onmousedown :: Text -> Attribute
onmousedown v = ("onmousedown", v)

-- | Set the @onmousemove@ attribute.
onmousemove :: Text -> Attribute
onmousemove v = ("onmousemove", v)

-- | Set the @onmouseout@ attribute.
onmouseout :: Text -> Attribute
onmouseout v = ("onmouseout", v)

-- | Set the @onmouseover@ attribute.
onmouseover :: Text -> Attribute
onmouseover v = ("onmouseover", v)

-- | Set the @onmouseup@ attribute.
onmouseup :: Text -> Attribute
onmouseup v = ("onmouseup", v)

-- | Set the @onreset@ attribute.
onreset :: Text -> Attribute
onreset v = ("onreset", v)

-- | Set the @onselect@ attribute.
onselect :: Text -> Attribute
onselect v = ("onselect", v)

-- | Set the @onsubmit@ attribute.
onsubmit :: Text -> Attribute
onsubmit v = ("onsubmit", v)

-- | Set the @onunload@ attribute.
onunload :: Text -> Attribute
onunload v = ("onunload", v)

-- | Set the @profile@ attribute.
profile :: Text -> Attribute
profile v = ("profile", v)

-- | Set the @prompt@ attribute.
prompt :: Text -> Attribute
prompt v = ("prompt", v)

-- | Set the @readonly@ attribute.
readonly :: Text -> Attribute
readonly v = ("readonly", v)

-- | Set the @rel@ attribute.
rel :: Text -> Attribute
rel v = ("rel", v)

-- | Set the @rev@ attribute.
rev :: Text -> Attribute
rev v = ("rev", v)

-- | Set the @rows@ attribute.
rows :: Text -> Attribute
rows v = ("rows", v)

-- | Set the @rowspan@ attribute.
rowspan :: Text -> Attribute
rowspan v = ("rowspan", v)

-- | Set the @rules@ attribute.
rules :: Text -> Attribute
rules v = ("rules", v)

-- | Set the @scheme@ attribute.
scheme :: Text -> Attribute
scheme v = ("scheme", v)

-- | Set the @scope@ attribute.
scope :: Text -> Attribute
scope v = ("scope", v)

-- | Set the @scrolling@ attribute.
scrolling :: Text -> Attribute
scrolling v = ("scrolling", v)

-- | Set the @selected@ attribute.
selected :: Text -> Attribute
selected v = ("selected", v)

-- | Set the @shape@ attribute.
shape :: Text -> Attribute
shape v = ("shape", v)

-- | Set the @size@ attribute.
size :: Text -> Attribute
size v = ("size", v)

-- | Set the @span@ attribute.
span :: Text -> Attribute
span v = ("span", v)

-- | Set the @src@ attribute.
src :: Text -> Attribute
src v = ("src", v)

-- | Set the @standby@ attribute.
standby :: Text -> Attribute
standby v = ("standby", v)

-- | Set the @start@ attribute.
start :: Text -> Attribute
start v = ("start", v)

-- | Set the @style@ attribute.
style :: Text -> Attribute
style v = ("style", v)

-- | Set the @summary@ attribute.
summary :: Text -> Attribute
summary v = ("summary", v)

-- | Set the @tabindex@ attribute.
tabindex :: Text -> Attribute
tabindex v = ("tabindex", v)

-- | Set the @target@ attribute.
target :: Text -> Attribute
target v = ("target", v)

-- | Set the @text@ attribute.
text :: Text -> Attribute
text v = ("text", v)

-- | Set the @title@ attribute.
title :: Text -> Attribute
title v = ("title", v)

-- | Set the @type_@ attribute.
type_ :: Text -> Attribute
type_ v = ("type", v)

-- | Set the @usemap@ attribute.
usemap :: Text -> Attribute
usemap v = ("usemap", v)

-- | Set the @valign@ attribute.
valign :: Text -> Attribute
valign v = ("valign", v)

-- | Set the @value@ attribute.
value :: Text -> Attribute
value v = ("value", v)

-- | Set the @valuetype@ attribute.
valuetype :: Text -> Attribute
valuetype v = ("valuetype", v)

-- | Set the @version@ attribute.
version :: Text -> Attribute
version v = ("version", v)

-- | Set the @vlink@ attribute.
vlink :: Text -> Attribute
vlink v = ("vlink", v)

-- | Set the @vspace@ attribute.
vspace :: Text -> Attribute
vspace v = ("vspace", v)

-- | Set the @width@ attribute.
width :: Text -> Attribute
width v = ("width", v)

-- | Set the @xmlns@ attribute.
xmlns :: Text -> Attribute
xmlns v = ("xmlns", v)
