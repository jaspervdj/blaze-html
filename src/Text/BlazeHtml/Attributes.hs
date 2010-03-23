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
    ) where

import Prelude ()

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html (Attribute)

-- | Set the @abbr@ attribute.
abbr :: Text -> Attribute
abbr value = ("abbr", value)

-- | Set the @accept_charset@ attribute.
accept_charset :: Text -> Attribute
accept_charset value = ("accept-charset", value)

-- | Set the @accept@ attribute.
accept :: Text -> Attribute
accept value = ("accept", value)

-- | Set the @accesskey@ attribute.
accesskey :: Text -> Attribute
accesskey value = ("accesskey", value)

-- | Set the @action@ attribute.
action :: Text -> Attribute
action value = ("action", value)

-- | Set the @align@ attribute.
align :: Text -> Attribute
align value = ("align", value)

-- | Set the @alink@ attribute.
alink :: Text -> Attribute
alink value = ("alink", value)

-- | Set the @alt@ attribute.
alt :: Text -> Attribute
alt value = ("alt", value)

-- | Set the @archive@ attribute.
archive :: Text -> Attribute
archive value = ("archive", value)

-- | Set the @axis@ attribute.
axis :: Text -> Attribute
axis value = ("axis", value)

-- | Set the @background@ attribute.
background :: Text -> Attribute
background value = ("background", value)

-- | Set the @bgcolor@ attribute.
bgcolor :: Text -> Attribute
bgcolor value = ("bgcolor", value)

-- | Set the @border@ attribute.
border :: Text -> Attribute
border value = ("border", value)

-- | Set the @cellpadding@ attribute.
cellpadding :: Text -> Attribute
cellpadding value = ("cellpadding", value)

-- | Set the @cellspacing@ attribute.
cellspacing :: Text -> Attribute
cellspacing value = ("cellspacing", value)

-- | Set the @char@ attribute.
char :: Text -> Attribute
char value = ("char", value)

-- | Set the @charoff@ attribute.
charoff :: Text -> Attribute
charoff value = ("charoff", value)

-- | Set the @charset@ attribute.
charset :: Text -> Attribute
charset value = ("charset", value)

-- | Set the @checked@ attribute.
checked :: Text -> Attribute
checked value = ("checked", value)

-- | Set the @cite@ attribute.
cite :: Text -> Attribute
cite value = ("cite", value)

-- | Set the @class_@ attribute.
class_ :: Text -> Attribute
class_ value = ("class", value)

-- | Set the @classid@ attribute.
classid :: Text -> Attribute
classid value = ("classid", value)

-- | Set the @clear@ attribute.
clear :: Text -> Attribute
clear value = ("clear", value)

-- | Set the @code@ attribute.
code :: Text -> Attribute
code value = ("code", value)

-- | Set the @codebase@ attribute.
codebase :: Text -> Attribute
codebase value = ("codebase", value)

-- | Set the @codetype@ attribute.
codetype :: Text -> Attribute
codetype value = ("codetype", value)

-- | Set the @color@ attribute.
color :: Text -> Attribute
color value = ("color", value)

-- | Set the @cols@ attribute.
cols :: Text -> Attribute
cols value = ("cols", value)

-- | Set the @colspan@ attribute.
colspan :: Text -> Attribute
colspan value = ("colspan", value)

-- | Set the @compact@ attribute.
compact :: Text -> Attribute
compact value = ("compact", value)

-- | Set the @content@ attribute.
content :: Text -> Attribute
content value = ("content", value)

-- | Set the @coords@ attribute.
coords :: Text -> Attribute
coords value = ("coords", value)

-- | Set the @data_@ attribute.
data_ :: Text -> Attribute
data_ value = ("data", value)

-- | Set the @datetime@ attribute.
datetime :: Text -> Attribute
datetime value = ("datetime", value)

-- | Set the @declare@ attribute.
declare :: Text -> Attribute
declare value = ("declare", value)

-- | Set the @defer@ attribute.
defer :: Text -> Attribute
defer value = ("defer", value)

-- | Set the @dir@ attribute.
dir :: Text -> Attribute
dir value = ("dir", value)

-- | Set the @disabled@ attribute.
disabled :: Text -> Attribute
disabled value = ("disabled", value)

-- | Set the @enctype@ attribute.
enctype :: Text -> Attribute
enctype value = ("enctype", value)

-- | Set the @face@ attribute.
face :: Text -> Attribute
face value = ("face", value)

-- | Set the @for@ attribute.
for :: Text -> Attribute
for value = ("for", value)

-- | Set the @frame@ attribute.
frame :: Text -> Attribute
frame value = ("frame", value)

-- | Set the @frameborder@ attribute.
frameborder :: Text -> Attribute
frameborder value = ("frameborder", value)

-- | Set the @headers@ attribute.
headers :: Text -> Attribute
headers value = ("headers", value)

-- | Set the @height@ attribute.
height :: Text -> Attribute
height value = ("height", value)

-- | Set the @href@ attribute.
href :: Text -> Attribute
href value = ("href", value)

-- | Set the @hreflang@ attribute.
hreflang :: Text -> Attribute
hreflang value = ("hreflang", value)

-- | Set the @hspace@ attribute.
hspace :: Text -> Attribute
hspace value = ("hspace", value)

-- | Set the @http_equiv@ attribute.
http_equiv :: Text -> Attribute
http_equiv value = ("http-equiv", value)

-- | Set the @id@ attribute.
id :: Text -> Attribute
id value = ("id", value)

-- | Set the @ismap@ attribute.
ismap :: Text -> Attribute
ismap value = ("ismap", value)

-- | Set the @label@ attribute.
label :: Text -> Attribute
label value = ("label", value)

-- | Set the @lang@ attribute.
lang :: Text -> Attribute
lang value = ("lang", value)

-- | Set the @language@ attribute.
language :: Text -> Attribute
language value = ("language", value)

-- | Set the @link@ attribute.
link :: Text -> Attribute
link value = ("link", value)

-- | Set the @longdesc@ attribute.
longdesc :: Text -> Attribute
longdesc value = ("longdesc", value)

-- | Set the @marginheight@ attribute.
marginheight :: Text -> Attribute
marginheight value = ("marginheight", value)

-- | Set the @marginwidth@ attribute.
marginwidth :: Text -> Attribute
marginwidth value = ("marginwidth", value)

-- | Set the @maxlength@ attribute.
maxlength :: Text -> Attribute
maxlength value = ("maxlength", value)

-- | Set the @media@ attribute.
media :: Text -> Attribute
media value = ("media", value)

-- | Set the @method@ attribute.
method :: Text -> Attribute
method value = ("method", value)

-- | Set the @multiple@ attribute.
multiple :: Text -> Attribute
multiple value = ("multiple", value)

-- | Set the @name@ attribute.
name :: Text -> Attribute
name value = ("name", value)

-- | Set the @nohref@ attribute.
nohref :: Text -> Attribute
nohref value = ("nohref", value)

-- | Set the @noresize@ attribute.
noresize :: Text -> Attribute
noresize value = ("noresize", value)

-- | Set the @noshade@ attribute.
noshade :: Text -> Attribute
noshade value = ("noshade", value)

-- | Set the @nowrap@ attribute.
nowrap :: Text -> Attribute
nowrap value = ("nowrap", value)

-- | Set the @object@ attribute.
object :: Text -> Attribute
object value = ("object", value)

-- | Set the @onblur@ attribute.
onblur :: Text -> Attribute
onblur value = ("onblur", value)

-- | Set the @onchange@ attribute.
onchange :: Text -> Attribute
onchange value = ("onchange", value)

-- | Set the @onclick@ attribute.
onclick :: Text -> Attribute
onclick value = ("onclick", value)

-- | Set the @ondblclick@ attribute.
ondblclick :: Text -> Attribute
ondblclick value = ("ondblclick", value)

-- | Set the @onfocus@ attribute.
onfocus :: Text -> Attribute
onfocus value = ("onfocus", value)

-- | Set the @onkeydown@ attribute.
onkeydown :: Text -> Attribute
onkeydown value = ("onkeydown", value)

-- | Set the @onkeypress@ attribute.
onkeypress :: Text -> Attribute
onkeypress value = ("onkeypress", value)

-- | Set the @onkeyup@ attribute.
onkeyup :: Text -> Attribute
onkeyup value = ("onkeyup", value)

-- | Set the @onload@ attribute.
onload :: Text -> Attribute
onload value = ("onload", value)

-- | Set the @onmousedown@ attribute.
onmousedown :: Text -> Attribute
onmousedown value = ("onmousedown", value)

-- | Set the @onmousemove@ attribute.
onmousemove :: Text -> Attribute
onmousemove value = ("onmousemove", value)

-- | Set the @onmouseout@ attribute.
onmouseout :: Text -> Attribute
onmouseout value = ("onmouseout", value)

-- | Set the @onmouseover@ attribute.
onmouseover :: Text -> Attribute
onmouseover value = ("onmouseover", value)

-- | Set the @onmouseup@ attribute.
onmouseup :: Text -> Attribute
onmouseup value = ("onmouseup", value)

-- | Set the @onreset@ attribute.
onreset :: Text -> Attribute
onreset value = ("onreset", value)

-- | Set the @onselect@ attribute.
onselect :: Text -> Attribute
onselect value = ("onselect", value)

-- | Set the @onsubmit@ attribute.
onsubmit :: Text -> Attribute
onsubmit value = ("onsubmit", value)

-- | Set the @onunload@ attribute.
onunload :: Text -> Attribute
onunload value = ("onunload", value)

-- | Set the @profile@ attribute.
profile :: Text -> Attribute
profile value = ("profile", value)

-- | Set the @prompt@ attribute.
prompt :: Text -> Attribute
prompt value = ("prompt", value)

-- | Set the @readonly@ attribute.
readonly :: Text -> Attribute
readonly value = ("readonly", value)

-- | Set the @rel@ attribute.
rel :: Text -> Attribute
rel value = ("rel", value)

-- | Set the @rev@ attribute.
rev :: Text -> Attribute
rev value = ("rev", value)

-- | Set the @rows@ attribute.
rows :: Text -> Attribute
rows value = ("rows", value)

-- | Set the @rowspan@ attribute.
rowspan :: Text -> Attribute
rowspan value = ("rowspan", value)

-- | Set the @rules@ attribute.
rules :: Text -> Attribute
rules value = ("rules", value)

-- | Set the @scheme@ attribute.
scheme :: Text -> Attribute
scheme value = ("scheme", value)

-- | Set the @scope@ attribute.
scope :: Text -> Attribute
scope value = ("scope", value)

-- | Set the @scrolling@ attribute.
scrolling :: Text -> Attribute
scrolling value = ("scrolling", value)

-- | Set the @selected@ attribute.
selected :: Text -> Attribute
selected value = ("selected", value)

-- | Set the @shape@ attribute.
shape :: Text -> Attribute
shape value = ("shape", value)

-- | Set the @size@ attribute.
size :: Text -> Attribute
size value = ("size", value)

-- | Set the @span@ attribute.
span :: Text -> Attribute
span value = ("span", value)

-- | Set the @src@ attribute.
src :: Text -> Attribute
src value = ("src", value)

-- | Set the @standby@ attribute.
standby :: Text -> Attribute
standby value = ("standby", value)

-- | Set the @start@ attribute.
start :: Text -> Attribute
start value = ("start", value)

-- | Set the @style@ attribute.
style :: Text -> Attribute
style value = ("style", value)

-- | Set the @summary@ attribute.
summary :: Text -> Attribute
summary value = ("summary", value)

-- | Set the @tabindex@ attribute.
tabindex :: Text -> Attribute
tabindex value = ("tabindex", value)

-- | Set the @target@ attribute.
target :: Text -> Attribute
target value = ("target", value)

-- | Set the @text@ attribute.
text :: Text -> Attribute
text value = ("text", value)

-- | Set the @title@ attribute.
title :: Text -> Attribute
title value = ("title", value)

-- | Set the @type_@ attribute.
type_ :: Text -> Attribute
type_ value = ("type", value)

-- | Set the @usemap@ attribute.
usemap :: Text -> Attribute
usemap value = ("usemap", value)

-- | Set the @valign@ attribute.
valign :: Text -> Attribute
valign value = ("valign", value)

-- | Set the @value@ attribute.
value :: Text -> Attribute
value value = ("value", value)

-- | Set the @valuetype@ attribute.
valuetype :: Text -> Attribute
valuetype value = ("valuetype", value)

-- | Set the @version@ attribute.
version :: Text -> Attribute
version value = ("version", value)

-- | Set the @vlink@ attribute.
vlink :: Text -> Attribute
vlink value = ("vlink", value)

-- | Set the @vspace@ attribute.
vspace :: Text -> Attribute
vspace value = ("vspace", value)

-- | Set the @width@ attribute.
width :: Text -> Attribute
width value = ("width", value)
