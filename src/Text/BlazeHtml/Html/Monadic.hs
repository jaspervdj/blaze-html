{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Like "Text.BlazeHtml.Text.Html", but using a monadic interface

module Text.BlazeHtml.Html.Monadic
    ( 
     -- * Basic combinators
     module Text.BlazeHtml.Internal.Html
    -- * Text chunks
    , text, emptyText
    -- * Elements
    , a_, a
    , abbr
    , acronym
    , address
    , applet
    , area
    , b
    , base
    , basefont
    , bdo
    , big
    , blockquote
    , body
    , br
    , button
    , caption
    , center
    , cite
    , code
    , col
    , colgroup
    , dd
    , del
    , dfn
    , dir
    , div
    , dl
    , dt
    , em
    , fieldset
    , font
    , form
    , frame
    , frameset
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
    , iframe
    , img_, img
    , input
    , ins
    , isindex
    , kbd
    , label
    , legend
    , li
    , link
    , map
    , menu
    , meta
    , noframes
    , noscript
    , object
    , ol
    , optgroup
    , option
    , p
    , param
    , pre
    , q
    , s
    , samp
    , script
    , select
    , small
    , span
    , strike
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
    , u
    , ul
    , var
    -- * Attributes
		, atAbbr
		, atAccept_charset
		, atAccept
		, atAccesskey
		, atAction
		, atAlign
		, atAlink
		, atAlt
		, atArchive
		, atAxis
		, atBackground
		, atBgcolor
		, atBorder
		, atCellpadding
		, atCellspacing
		, atChar
		, atCharoff
		, atCharset
		, atChecked
		, atCite
		, atClass
		, atClassid
		, atClear
		, atCode
		, atCodebase
		, atCodetype
		, atColor
		, atCols
		, atColspan
		, atCompact
		, atContent
		, atCoords
		, atData
		, atDatetime
		, atDeclare
		, atDefer
		, atDir
		, atDisabled
		, atEnctype
		, atFace
		, atFor
		, atFrame
		, atFrameborder
		, atHeaders
		, atHeight
		, atHref
		, atHreflang
		, atHspace
		, atHttp_equiv
		, atId
		, atIsmap
		, atLabel
		, atLang
		, atLanguage
		, atLink
		, atLongdesc
		, atMarginheight
		, atMarginwidth
		, atMaxlength
		, atMedia
		, atMethod
		, atMultiple
		, atName
		, atNohref
		, atNoresize
		, atNoshade
		, atNowrap
		, atObject
		, atOnblur
		, atOnchange
		, atOnclick
		, atOndblclick
		, atOnfocus
		, atOnkeydown
		, atOnkeypress
		, atOnkeyup
		, atOnload
		, atOnmousedown
		, atOnmousemove
		, atOnmouseout
		, atOnmouseover
		, atOnmouseup
		, atOnreset
		, atOnselect
		, atOnsubmit
		, atOnunload
		, atProfile
		, atPrompt
		, atReadonly
		, atRel
		, atRev
		, atRows
		, atRowspan
		, atRules
		, atScheme
		, atScope
		, atScrolling
		, atSelected
		, atShape
		, atSize
		, atSpan
		, atSrc
		, atStandby
		, atStart
		, atStyle
		, atSummary
		, atTabindex
		, atTarget
		, atText
		, atTitle
		, atType
		, atUsemap
		, atValign
		, atValue
		, atValuetype
		, atVersion
		, atVlink
		, atVspace    
    ) where

import Prelude hiding (div, head, span, map)
import Data.Monoid

import Data.Monoid (mempty)
import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Internal.HtmlMonad    
import Text.BlazeHtml.Internal.Escaping


-- HtmlMonad 

-- | Create an 'Html' value from a chunk of text, with proper string escaping.
text :: (Html h) => Text -> HtmlMonad h ()
text = unescapedText . escapeHtml

-- | 'emptyText' is an empty chunk of text with no tags.
emptyText :: (Html h) => HtmlMonad h ()
emptyText = mempty

-- Elements

-- | Render an @a@ element.
a_ :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
a_ = nodeElement "a"

a :: (Html h)=> Text -> HtmlMonad h () -> HtmlMonad h ()
a href = a_ <! ("href", href)

-- | Render a @abbr@ element
abbr :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
abbr = nodeElement "abbr"

-- | Render a @acronym@ element
acronym :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
acronym = nodeElement "acronym"

-- | Render a @address@ element
address :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
address = nodeElement "address"

-- | Render a @applet@ element
applet :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
applet = nodeElement "applet"

-- | Render a @b@ element
b :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
b = nodeElement "b"

-- | Render a @bdo@ element
bdo :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
bdo = nodeElement "bdo"

-- | Render a @big@ element
big :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
big = nodeElement "big"

-- | Render a @blockquote@ element
blockquote :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
blockquote = nodeElement "blockquote"

-- | Render a @body@ element
body :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
body = nodeElement "body"

-- | Render a @button@ element
button :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
button = nodeElement "button"

-- | Render a @caption@ element
caption :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
caption = nodeElement "caption"

-- | Render a @center@ element
center :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
center = nodeElement "center"

-- | Render a @cite@ element
cite :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
cite = nodeElement "cite"

-- | Render a @code@ element
code :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
code = nodeElement "code"

-- | Render a @colgroup@ element
colgroup :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
colgroup = nodeElement "colgroup"

-- | Render a @dd@ element
dd :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
dd = nodeElement "dd"

-- | Render a @del@ element
del :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
del = nodeElement "del"

-- | Render a @dfn@ element
dfn :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
dfn = nodeElement "dfn"

-- | Render a @dir@ element
dir :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
dir = nodeElement "dir"

-- | Render a @div@ element
div :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
div = nodeElement "div"

-- | Render a @dl@ element
dl :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
dl = nodeElement "dl"

-- | Render a @dt@ element
dt :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
dt = nodeElement "dt"

-- | Render a @em@ element
em :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
em = nodeElement "em"

-- | Render a @fieldset@ element
fieldset :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
fieldset = nodeElement "fieldset"

-- | Render a @font@ element
font :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
font = nodeElement "font"

-- | Render a @form@ element
form :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
form = nodeElement "form"

-- | Render a @frameset@ element
frameset :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
frameset = nodeElement "frameset"

-- | Render a @h1@ element
h1 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h1 = nodeElement "h1"

-- | Render a @h2@ element
h2 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h2 = nodeElement "h2"

-- | Render a @h3@ element
h3 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h3 = nodeElement "h3"

-- | Render a @h4@ element
h4 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h4 = nodeElement "h4"

-- | Render a @h5@ element
h5 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h5 = nodeElement "h5"

-- | Render a @h6@ element
h6 :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
h6 = nodeElement "h6"

-- | Render a @head@ element
head :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
head = nodeElement "head"

-- | Render a @html@ element
html :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
html = nodeElement "html"

-- | Render a @i@ element
i :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
i = nodeElement "i"

-- | Render a @iframe@ element
iframe :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
iframe = nodeElement "iframe"

-- | Render a @ins@ element
ins :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
ins = nodeElement "ins"

-- | Render a @kbd@ element
kbd :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
kbd = nodeElement "kbd"

-- | Render a @label@ element
label :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
label = nodeElement "label"

-- | Render a @legend@ element
legend :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
legend = nodeElement "legend"

-- | Render a @li@ element
li :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
li = nodeElement "li"

-- | Render a @map@ element
map :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
map = nodeElement "map"

-- | Render a @menu@ element
menu :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
menu = nodeElement "menu"

-- | Render a @noframes@ element
noframes :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
noframes = nodeElement "noframes"

-- | Render a @noscript@ element
noscript :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
noscript = nodeElement "noscript"

-- | Render a @object@ element
object :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
object = nodeElement "object"

-- | Render a @ol@ element
ol :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
ol = nodeElement "ol"

-- | Render a @optgroup@ element
optgroup :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
optgroup = nodeElement "optgroup"

-- | Render a @option@ element
option :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
option = nodeElement "option"

-- | Render a @p@ element
p :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
p = nodeElement "p"

-- | Render a @pre@ element
pre :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
pre = nodeElement "pre"

-- | Render a @q@ element
q :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
q = nodeElement "q"

-- | Render a @s@ element
s :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
s = nodeElement "s"

-- | Render a @samp@ element
samp :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
samp = nodeElement "samp"

-- | Render a @script@ element
script :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
script = nodeElement "script"

-- | Render a @select@ element
select :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
select = nodeElement "select"

-- | Render a @small@ element
small :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
small = nodeElement "small"

-- | Render a @span@ element
span :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
span = nodeElement "span"

-- | Render a @strike@ element
strike :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
strike = nodeElement "strike"

-- | Render a @strong@ element
strong :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
strong = nodeElement "strong"

-- | Render a @style@ element
style :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
style = nodeElement "style"

-- | Render a @sub@ element
sub :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
sub = nodeElement "sub"

-- | Render a @sup@ element
sup :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
sup = nodeElement "sup"

-- | Render a @table@ element
table :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
table = nodeElement "table"

-- | Render a @tbody@ element
tbody :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
tbody = nodeElement "tbody"

-- | Render a @td@ element
td :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
td = nodeElement "td"

-- | Render a @textarea@ element
textarea :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
textarea = nodeElement "textarea"

-- | Render a @tfoot@ element
tfoot :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
tfoot = nodeElement "tfoot"

-- | Render a @th@ element
th :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
th = nodeElement "th"

-- | Render a @thead@ element
thead :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
thead = nodeElement "thead"

-- | Render a @title@ element
title :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
title = nodeElement "title"

-- | Render a @tr@ element
tr :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
tr = nodeElement "tr"

-- | Render a @tt@ element
tt :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
tt = nodeElement "tt"

-- | Render a @u@ element
u :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
u = nodeElement "u"

-- | Render a @ul@ element
ul :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
ul = nodeElement "ul"

-- | Render a @var@ element
var :: (Html h)=> HtmlMonad h () -> HtmlMonad h ()
var = nodeElement "var"


-- | Render a @area@ leaf element.
area :: (Html h)=> HtmlMonad h ()
area = leafElement "area"

-- | Render a @base@ leaf element.
base :: (Html h)=> HtmlMonad h ()
base = leafElement "base"

-- | Render a @basefont@ leaf element.
basefont :: (Html h)=> HtmlMonad h ()
basefont = leafElement "basefont"

-- | Render a @br@ leaf element.
br :: (Html h)=> HtmlMonad h ()
br = leafElement "br"

-- | Render a @col@ leaf element.
col :: (Html h)=> HtmlMonad h ()
col = leafElement "col"

-- | Render a @frame@ leaf element.
frame :: (Html h)=> HtmlMonad h ()
frame = leafElement "frame"

-- | Render a @hr@ leaf element.
hr :: (Html h)=> HtmlMonad h ()
hr = leafElement "hr"

-- | Render a @img@ leaf element, without any attribute
img_ :: (Html h)=> HtmlMonad h ()
img_ = leafElement "img"

-- | Render an @img@ element with mandatory data
img :: (Html h)=> Text -> Text -> HtmlMonad h ()
img src alt = img_ !: [("src", src), ("alt", alt)]

-- | Render a @input@ leaf element.
input :: (Html h)=> HtmlMonad h ()
input = leafElement "input"

-- | Render a @isindex@ leaf element.
isindex :: (Html h)=> HtmlMonad h ()
isindex = leafElement "isindex"

-- | Render a @link@ leaf element.
link :: (Html h)=> HtmlMonad h ()
link = leafElement "link"

-- | Render a @meta@ leaf element.
meta :: (Html h)=> HtmlMonad h ()
meta = leafElement "meta"

-- | Render a @param@ leaf element.
param :: (Html h)=> HtmlMonad h ()
param = leafElement "param"

-- Attributes

-- | Build a @abbr@ attribute (%Text; #IMPLIED)
atAbbr :: Text -> Attribute
atAbbr val = ("abbr", val)

-- | Build a @accept-carset@ attribute (%Charsets; #IMPLIED)
atAccept_charset :: Text -> Attribute
atAccept_charset val = ("accept-charset", val)

-- | Build a @accept@ attribute (%ContentTypes; #IMPLIED)
atAccept :: Text -> Attribute
atAccept val = ("accept", val)

-- | Build a @accesskey@ attribute (%Character; #IMPLIED)
atAccesskey :: Text -> Attribute
atAccesskey val = ("accesskey", val)

-- | Build a @action@ attribute (%URI; #REQUIRED)
atAction :: Text -> Attribute
atAction val = ("action", val)

-- | Build a @align@ attribute (%Align; #IMPLIED)
atAlign :: Text -> Attribute
atAlign val = ("align", val)

-- | Build a @alink@ attribute (%Color; #IMPLIED)
atAlink :: Text -> Attribute
atAlink val = ("alink", val)

-- | Build a @alt@ attribute (%Text; #IMPLIED)
atAlt :: Text -> Attribute
atAlt val = ("alt", val)

-- | Build a @archive@ attribute (%CDATA  #IMPLIED)
atArchive :: Text -> Attribute
atArchive val = ("archive", val)

-- | Build a @axis@ attribute (%CDATA  #IMPLIED)
atAxis :: Text -> Attribute
atAxis val = ("axis", val)

-- | Build a @background@ attribute (%URI; #IMPLIED)
atBackground :: Text -> Attribute
atBackground val = ("background", val)

-- | Build a @bgcolor@ attribute (%Color; #IMPLIED)
atBgcolor :: Text -> Attribute
atBgcolor val = ("bgcolor", val)

-- | Build a @border@ attribute (%Pixels; #IMPLIED)
atBorder :: Text -> Attribute
atBorder val = ("border", val)

-- | Build a @cellpadding@ attribute (%Length; #IMPLIED)
atCellpadding :: Text -> Attribute
atCellpadding val = ("cellpadding", val)

-- | Build a @cellspacing@ attribute (%Length; #IMPLIED)
atCellspacing :: Text -> Attribute
atCellspacing val = ("cellspacing", val)

-- | Build a @char@ attribute (%Character; #IMPLIED)
atChar :: Text -> Attribute
atChar val = ("char", val)

-- | Build a @charoff@ attribute (%Length; #IMPLIED)
atCharoff :: Text -> Attribute
atCharoff val = ("charoff", val)

-- | Build a @charset@ attribute (%Charset; #IMPLIED)
atCharset :: Text -> Attribute
atCharset val = ("charset", val)

-- | Build a @checked@ attribute (%checked  #IMPLIED)
atChecked :: Text -> Attribute
atChecked val = ("checked", val)

-- | Build a @cite@ attribute (%URI; #IMPLIED)
atCite :: Text -> Attribute
atCite val = ("cite", val)

-- | Build a @class@ attribute (%CDATA  #IMPLIED)
atClass :: Text -> Attribute
atClass val = ("class", val)

-- | Build a @classid@ attribute (%URI; #IMPLIED)
atClassid :: Text -> Attribute
atClassid val = ("classid", val)

-- | Build a @clear@ attribute ((left | all | right | none))
atClear :: Text -> Attribute
atClear val = ("clear", val)

-- | Build a @code@ attribute (%CDATA  #IMPLIED)
atCode :: Text -> Attribute
atCode val = ("code", val)

-- | Build a @codebase@ attribute (%URI; #IMPLIED)
atCodebase :: Text -> Attribute
atCodebase val = ("codebase", val)

-- | Build a @codetype@ attribute (%ContentType; #IMPLIED)
atCodetype :: Text -> Attribute
atCodetype val = ("codetype", val)

-- | Build a @color@ attribute (%Color; #IMPLIED)
atColor :: Text -> Attribute
atColor val = ("color", val)

-- | Build a @cols@ attribute (%MultiLengths; #IMPLIED)
atCols :: Text -> Attribute
atCols val = ("cols", val)

-- | Build a @colspan@ attribute (%number 1)
atColspan :: Text -> Attribute
atColspan val = ("colspan", val)

-- | Build a @compact@ attribute ((compact) #IMPLIED)
atCompact :: Text -> Attribute
atCompact val = ("compact", val)

-- | Build a @content@ attribute (%CDATA  #REQUIRED)
atContent :: Text -> Attribute
atContent val = ("content", val)

-- | Build a @coords@ attribute (%Coords; #IMPLIED)
atCoords :: Text -> Attribute
atCoords val = ("coords", val)

-- | Build a @data@ attribute (%URI; #IMPLIED)
atData :: Text -> Attribute
atData val = ("data", val)

-- | Build a @datetime@ attribute (%Datetime; #IMPLIED)
atDatetime :: Text -> Attribute
atDatetime val = ("datetime", val)

-- | Build a @declare@ attribute ((declare) #IMPLIED)
atDeclare :: Text -> Attribute
atDeclare val = ("declare", val)

-- | Build a @defer@ attribute ((defer) #IMPLIED)
atDefer :: Text -> Attribute
atDefer val = ("defer", val)

-- | Build a @dir@ attribute ((ltr | rtl) #REQUIRED)
atDir :: Text -> Attribute
atDir val = ("dir", val)

-- | Build a @disabled@ attribute ((disabled) #IMPLIED)
atDisabled :: Text -> Attribute
atDisabled val = ("disabled", val)

-- | Build a @enctype@ attribute (%ContentType; "application/x-www- form-urlencoded")
atEnctype :: Text -> Attribute
atEnctype val = ("enctype", val)

-- | Build a @face@ attribute (%CDATA  #IMPLIED)
atFace :: Text -> Attribute
atFace val = ("face", val)

-- | Build a @for@ attribute (%IDREF  #IMPLIED)
atFor :: Text -> Attribute
atFor val = ("for", val)

-- | Build a @frame@ attribute (%TFrame; #IMPLIED)
atFrame :: Text -> Attribute
atFrame val = ("frame", val)

-- | Build a @frameborder@ attribute ((1 | 0)  1)
atFrameborder :: Text -> Attribute
atFrameborder val = ("frameborder", val)

-- | Build a @headers@ attribute (%IDREFS  #IMPLIED)
atHeaders :: Text -> Attribute
atHeaders val = ("headers", val)

-- | Build a @height@ attribute (%Length; #IMPLIED)
atHeight :: Text -> Attribute
atHeight val = ("height", val)

-- | Build a @href@ attribute (%URI; #IMPLIED)
atHref :: Text -> Attribute
atHref val = ("href", val)

-- | Build a @hreflang@ attribute (%LanguageCode; #IMPLIED)
atHreflang :: Text -> Attribute
atHreflang val = ("hreflang", val)

-- | Build a @hspace@ attribute (%Pixels; #IMPLIED)
atHspace :: Text -> Attribute
atHspace val = ("hspace", val)

-- | Build a @http-equiv@ attribute (%NAME  #IMPLIED)
atHttp_equiv :: Text -> Attribute
atHttp_equiv val = ("http-equiv", val)

-- | Build a @id@ attribute (%ID  #IMPLIED)
atId :: Text -> Attribute
atId val = ("id", val)

-- | Build a @ismap@ attribute ((ismap) #IMPLIED)
atIsmap :: Text -> Attribute
atIsmap val = ("ismap", val)

-- | Build a @label@ attribute (%Text; #IMPLIED)
atLabel :: Text -> Attribute
atLabel val = ("label", val)

-- | Build a @lang@ attribute (%LanguageCode; #IMPLIED)
atLang :: Text -> Attribute
atLang val = ("lang", val)

-- | Build a @language@ attribute (%CDATA  #IMPLIED)
atLanguage :: Text -> Attribute
atLanguage val = ("language", val)

-- | Build a @link@ attribute (%Color; #IMPLIED)
atLink :: Text -> Attribute
atLink val = ("link", val)

-- | Build a @longdesc@ attribute (%URI; #IMPLIED)
atLongdesc :: Text -> Attribute
atLongdesc val = ("longdesc", val)

-- | Build a @marginheight@ attribute (%Pixels; #IMPLIED)
atMarginheight :: Text -> Attribute
atMarginheight val = ("marginheight", val)

-- | Build a @marginwidth@ attribute (%Pixels; #IMPLIED)
atMarginwidth :: Text -> Attribute
atMarginwidth val = ("marginwidth", val)

-- | Build a @maxlength@ attribute (%NUMBER  #IMPLIED)
atMaxlength :: Text -> Attribute
atMaxlength val = ("maxlength", val)

-- | Build a @media@ attribute (%MediaDesc; #IMPLIED)
atMedia :: Text -> Attribute
atMedia val = ("media", val)

-- | Build a @method@ attribute ((GET | POST)  GET)
atMethod :: Text -> Attribute
atMethod val = ("method", val)

-- | Build a @multiple@ attribute ((multiple) #IMPLIED)
atMultiple :: Text -> Attribute
atMultiple val = ("multiple", val)

-- | Build a @name@ attribute (%CDATA  #IMPLIED)
atName :: Text -> Attribute
atName val = ("name", val)

-- | Build a @nohref@ attribute ((nohref) #IMPLIED)
atNohref :: Text -> Attribute
atNohref val = ("nohref", val)

-- | Build a @noresize@ attribute ((noresize) #IMPLIED)
atNoresize :: Text -> Attribute
atNoresize val = ("noresize", val)

-- | Build a @noshade@ attribute ((noshade) #IMPLIED)
atNoshade :: Text -> Attribute
atNoshade val = ("noshade", val)

-- | Build a @nowrap@ attribute ((nowrap) #IMPLIED)
atNowrap :: Text -> Attribute
atNowrap val = ("nowrap", val)

-- | Build a @object@ attribute (%CDATA  #IMPLIED)
atObject :: Text -> Attribute
atObject val = ("object", val)

-- | Build a @onblur@ attribute (%Script; #IMPLIED)
atOnblur :: Text -> Attribute
atOnblur val = ("onblur", val)

-- | Build a @onchange@ attribute (%Script; #IMPLIED)
atOnchange :: Text -> Attribute
atOnchange val = ("onchange", val)

-- | Build a @onclick@ attribute (%Script; #IMPLIED)
atOnclick :: Text -> Attribute
atOnclick val = ("onclick", val)

-- | Build a @ondblclick@ attribute (%Script; #IMPLIED)
atOndblclick :: Text -> Attribute
atOndblclick val = ("ondblclick", val)

-- | Build a @onfocus@ attribute (%Script; #IMPLIED)
atOnfocus :: Text -> Attribute
atOnfocus val = ("onfocus", val)

-- | Build a @onkeydown@ attribute (%Script; #IMPLIED)
atOnkeydown :: Text -> Attribute
atOnkeydown val = ("onkeydown", val)

-- | Build a @onkeypress@ attribute (%Script; #IMPLIED)
atOnkeypress :: Text -> Attribute
atOnkeypress val = ("onkeypress", val)

-- | Build a @onkeyup@ attribute (%Script; #IMPLIED)
atOnkeyup :: Text -> Attribute
atOnkeyup val = ("onkeyup", val)

-- | Build a @onload@ attribute (%Script; #IMPLIED)
atOnload :: Text -> Attribute
atOnload val = ("onload", val)

-- | Build a @onmousedown@ attribute (%Script; #IMPLIED)
atOnmousedown :: Text -> Attribute
atOnmousedown val = ("onmousedown", val)

-- | Build a @onmousemove@ attribute (%Script; #IMPLIED)
atOnmousemove :: Text -> Attribute
atOnmousemove val = ("onmousemove", val)

-- | Build a @onmouseout@ attribute (%Script; #IMPLIED)
atOnmouseout :: Text -> Attribute
atOnmouseout val = ("onmouseout", val)

-- | Build a @onmouseover@ attribute (%Script; #IMPLIED)
atOnmouseover :: Text -> Attribute
atOnmouseover val = ("onmouseover", val)

-- | Build a @onmouseup@ attribute (%Script; #IMPLIED)
atOnmouseup :: Text -> Attribute
atOnmouseup val = ("onmouseup", val)

-- | Build a @onreset@ attribute (%Script; #IMPLIED)
atOnreset :: Text -> Attribute
atOnreset val = ("onreset", val)

-- | Build a @onselect@ attribute (%Script; #IMPLIED)
atOnselect :: Text -> Attribute
atOnselect val = ("onselect", val)

-- | Build a @onsubmit@ attribute (%Script; #IMPLIED)
atOnsubmit :: Text -> Attribute
atOnsubmit val = ("onsubmit", val)

-- | Build a @onunload@ attribute (%Script; #IMPLIED)
atOnunload :: Text -> Attribute
atOnunload val = ("onunload", val)

-- | Build a @profile@ attribute (%URI; #IMPLIED)
atProfile :: Text -> Attribute
atProfile val = ("profile", val)

-- | Build a @prompt@ attribute (%Text; #IMPLIED)
atPrompt :: Text -> Attribute
atPrompt val = ("prompt", val)

-- | Build a @readonly@ attribute ((readonly) #IMPLIED)
atReadonly :: Text -> Attribute
atReadonly val = ("readonly", val)

-- | Build a @rel@ attribute (%LinkTypes; #IMPLIED)
atRel :: Text -> Attribute
atRel val = ("rel", val)

-- | Build a @rev@ attribute (%LinkTypes; #IMPLIED)
atRev :: Text -> Attribute
atRev val = ("rev", val)

-- | Build a @rows@ attribute (%MultiLengths; #IMPLIED)
atRows :: Text -> Attribute
atRows val = ("rows", val)

-- | Build a @rowspan@ attribute (%NUMBER  1)
atRowspan :: Text -> Attribute
atRowspan val = ("rowspan", val)

-- | Build a @rules@ attribute ( TABLE  %TRules; #IMPLIED)
atRules :: Text -> Attribute
atRules val = ("rules", val)

-- | Build a @scheme@ attribute (%CDATA  #IMPLIED)
atScheme :: Text -> Attribute
atScheme val = ("scheme", val)

-- | Build a @scope@ attribute (%Scope; #IMPLIED)
atScope :: Text -> Attribute
atScope val = ("scope", val)

-- | Build a @scrolling@ attribute ((yes | no | auto)  auto)
atScrolling :: Text -> Attribute
atScrolling val = ("scrolling", val)

-- | Build a @selected@ attribute ((selected) #IMPLIED)
atSelected :: Text -> Attribute
atSelected val = ("selected", val)

-- | Build a @shape@ attribute (%Shape;  rect)
atShape :: Text -> Attribute
atShape val = ("shape", val)

-- | Build a @size@ attribute (%CDATA  #IMPLIED)
atSize :: Text -> Attribute
atSize val = ("size", val)

-- | Build a @span@ attribute (%NUMBER  1)
atSpan :: Text -> Attribute
atSpan val = ("span", val)

-- | Build a @src@ attribute (%URI; #IMPLIED)
atSrc :: Text -> Attribute
atSrc val = ("src", val)

-- | Build a @standby@ attribute (%Text; #IMPLIED)
atStandby :: Text -> Attribute
atStandby val = ("standby", val)

-- | Build a @start@ attribute (%NUMBER  #IMPLIED)
atStart :: Text -> Attribute
atStart val = ("start", val)

-- | Build a @style@ attribute (%StyleSheet; #IMPLIED)
atStyle :: Text -> Attribute
atStyle val = ("style", val)

-- | Build a @summary@ attribute (%Text; #IMPLIED)
atSummary :: Text -> Attribute
atSummary val = ("summary", val)

-- | Build a @tabindex@ attribute (%NUMBER  #IMPLIED)
atTabindex :: Text -> Attribute
atTabindex val = ("tabindex", val)

-- | Build a @target@ attribute (%FrameTarget; #IMPLIED)
atTarget :: Text -> Attribute
atTarget val = ("target", val)

-- | Build a @text@ attribute (%Color; #IMPLIED)
atText :: Text -> Attribute
atText val = ("text", val)

-- | Build a @title@ attribute (%Text; #IMPLIED)
atTitle :: Text -> Attribute
atTitle val = ("title", val)

-- | Build a @type@ attribute (%ContentType; #IMPLIED)
atType :: Text -> Attribute
atType val = ("type", val)

-- | Build a @usemap@ attribute (%URI; #IMPLIED)
atUsemap :: Text -> Attribute
atUsemap val = ("usemap", val)

-- | Build a @valign@ attribute ((top | middle | bottom | baseline) #IMPLIED)
atValign :: Text -> Attribute
atValign val = ("valign", val)

-- | Build a @value@ attribute (%CDATA  #IMPLIED)
atValue :: Text -> Attribute
atValue val = ("value", val)

-- | Build a @valuetype@ attribute ((DATA | REF | OBJECT)  DATA)
atValuetype :: Text -> Attribute
atValuetype val = ("valuetype", val)

-- | Build a @version@ attribute (%CDATA  %HTML.Version;  D   L   Constant)
atVersion :: Text -> Attribute
atVersion val = ("version", val)

-- | Build a @vlink@ attribute ( BODY  %Color; #IMPLIED)
atVlink :: Text -> Attribute
atVlink val = ("vlink", val)

-- | Build a @vspace@ attribute (%Pixels; #IMPLIED)
atVspace :: Text -> Attribute
atVspace val = ("vspace", val)

-- | Build a @width@ attribute (%Length; #IMPLIED)
atWidth :: Text -> Attribute
atWidth val = ("width", val)


