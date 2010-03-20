{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Text.BlazeHtml.Text.Html
where

import Text.BlazeHtml.Internal.Html
import qualified Data.Text as T
import Data.Monoid

infixr 2 +++  -- combining Html
infixr 7 <<   -- nesting Html
infixl 8 !    -- adding optional arguments

class (Html h)=> HTML a h where
    toHtml         :: a -> h
    toHtmlFromList :: [a] -> h

    toHtmlFromList xs = mconcat $ map toHtml xs

instance (Html h)=> HTML h h where
    toHtml a    = a

instance (Html h)=> HTML Char h where
    toHtml         a   = renderUnescapedText $ T.singleton a
    toHtmlFromList []  = mempty
    toHtmlFromList str = renderUnescapedText . T.pack  $ stringToHtmlString str

--instance (Html h, HTML a h) => HTML [a] h where
--    toHtml xs = toHtmlFromList xs

data HtmlAttr = HtmlAttr String String

class ADDATTRS a where
    (!) :: a -> [HtmlAttr] -> a

instance (ADDATTRS b) => ADDATTRS (a -> b) where
    fn ! attr = \ arg -> fn arg ! attr

instance (Html h)=> ADDATTRS h where
    html ! attrs = modifyUnescapedAttributes 
                      (const [(T.pack key, T.pack val) 
                                  | HtmlAttr key val <- attrs]) 
                      html

(<<) :: (Html h, HTML a h) => (h -> b) -> a -> b
fn << arg = fn (toHtml arg)

concatHtml :: (Html h, HTML a h) => [a] -> h
concatHtml as = mconcat (map toHtml as)

(+++) :: (Html h, HTML a h, HTML b h) => a -> b -> h
a +++ b = toHtml a `mappend` toHtml b

noHtml :: (Html h)=> h
noHtml = mempty


tag  :: (Html h)=> String -> h -> h
tag str htmls = renderElement (T.pack str) htmls

itag :: (Html h)=> String -> h
itag str = renderLeafElement (T.pack str) 

emptyAttr :: String -> HtmlAttr
emptyAttr s = HtmlAttr s ""

intAttr :: String -> Int -> HtmlAttr
intAttr s i = HtmlAttr s (show i)

strAttr :: String -> String -> HtmlAttr
strAttr s t = HtmlAttr s t


-- Processing Strings into Html friendly things.
-- This converts a String to a Html String.
stringToHtmlString :: String -> String
stringToHtmlString = concatMap fixChar
    where
      fixChar '<' = "&lt;"
      fixChar '>' = "&gt;"
      fixChar '&' = "&amp;"
      fixChar '"' = "&quot;"
      fixChar c   = [c]               


type URL = String

-- ---------------------------------------------------------------------------
-- Basic primitives

-- This is not processed for special chars. 
-- use stringToHtml or lineToHtml instead, for user strings, 
-- because they  understand special chars, like '<'.

primHtml :: (Html h)=> String -> h
primHtml x = renderUnescapedText $ T.pack x

-- ---------------------------------------------------------------------------
-- Basic Combinators

stringToHtml :: (Html h)=> String -> h
stringToHtml = primHtml . stringToHtmlString 

-- This converts a string, but keeps spaces as non-line-breakable

lineToHtml :: (Html h)=> String -> h
lineToHtml = primHtml . concatMap htmlizeChar2 . stringToHtmlString 
   where 
      htmlizeChar2 ' ' = "&nbsp;"
      htmlizeChar2 c   = [c]

-- ---------------------------------------------------------------------------
-- Html Constructors

-- (automatically generated)

address             :: (Html h)=> h -> h
anchor              :: (Html h)=> h -> h
applet              :: (Html h)=> h -> h
area                :: (Html h)=> h
basefont            :: (Html h)=> h
big                 :: (Html h)=> h -> h
blockquote          :: (Html h)=> h -> h
body                :: (Html h)=> h -> h
bold                :: (Html h)=> h -> h
br                  :: (Html h)=> h
caption             :: (Html h)=> h -> h
center              :: (Html h)=> h -> h
cite                :: (Html h)=> h -> h
ddef                :: (Html h)=> h -> h
define              :: (Html h)=> h -> h
dlist               :: (Html h)=> h -> h
dterm               :: (Html h)=> h -> h
emphasize           :: (Html h)=> h -> h
fieldset            :: (Html h)=> h -> h
font                :: (Html h)=> h -> h
form                :: (Html h)=> h -> h
frame               :: (Html h)=> h -> h
frameset            :: (Html h)=> h -> h
h1                  :: (Html h)=> h -> h
h2                  :: (Html h)=> h -> h
h3                  :: (Html h)=> h -> h
h4                  :: (Html h)=> h -> h
h5                  :: (Html h)=> h -> h
h6                  :: (Html h)=> h -> h
header              :: (Html h)=> h -> h
hr                  :: (Html h)=> h
image               :: (Html h)=> h
input               :: (Html h)=> h
italics             :: (Html h)=> h -> h
keyboard            :: (Html h)=> h -> h
legend              :: (Html h)=> h -> h
li                  :: (Html h)=> h -> h
meta                :: (Html h)=> h
noframes            :: (Html h)=> h -> h
olist               :: (Html h)=> h -> h
option              :: (Html h)=> h -> h
paragraph           :: (Html h)=> h -> h
param               :: (Html h)=> h
pre                 :: (Html h)=> h -> h
sample              :: (Html h)=> h -> h
select              :: (Html h)=> h -> h
small               :: (Html h)=> h -> h
strong              :: (Html h)=> h -> h
style               :: (Html h)=> h -> h
sub                 :: (Html h)=> h -> h
sup                 :: (Html h)=> h -> h
table               :: (Html h)=> h -> h
td                  :: (Html h)=> h -> h
textarea            :: (Html h)=> h -> h
th                  :: (Html h)=> h -> h
thebase             :: (Html h)=> h
thecode             :: (Html h)=> h -> h
thediv              :: (Html h)=> h -> h
thehtml             :: (Html h)=> h -> h
thelink             :: (Html h)=> h -> h
themap              :: (Html h)=> h -> h
thespan             :: (Html h)=> h -> h
thetitle            :: (Html h)=> h -> h
tr                  :: (Html h)=> h -> h
tt                  :: (Html h)=> h -> h
ulist               :: (Html h)=> h -> h
underline           :: (Html h)=> h -> h
variable            :: (Html h)=> h -> h

address             =  tag "ADDRESS"
anchor              =  tag "A"
applet              =  tag "APPLET"
area                = itag "AREA"
basefont            = itag "BASEFONT"
big                 =  tag "BIG"
blockquote          =  tag "BLOCKQUOTE"
body                =  tag "BODY"
bold                =  tag "B"
br                  = itag "BR"
caption             =  tag "CAPTION"
center              =  tag "CENTER"
cite                =  tag "CITE"
ddef                =  tag "DD"
define              =  tag "DFN"
dlist               =  tag "DL"
dterm               =  tag "DT"
emphasize           =  tag "EM"
fieldset            =  tag "FIELDSET"
font                =  tag "FONT"
form                =  tag "FORM"
frame               =  tag "FRAME"
frameset            =  tag "FRAMESET"
h1                  =  tag "H1"
h2                  =  tag "H2"
h3                  =  tag "H3"
h4                  =  tag "H4"
h5                  =  tag "H5"
h6                  =  tag "H6"
header              =  tag "HEAD"
hr                  = itag "HR"
image               = itag "IMG"
input               = itag "INPUT"
italics             =  tag "I"
keyboard            =  tag "KBD"
legend              =  tag "LEGEND"
li                  =  tag "LI"
meta                = itag "META"
noframes            =  tag "NOFRAMES"
olist               =  tag "OL"
option              =  tag "OPTION"
paragraph           =  tag "P"
param               = itag "PARAM"
pre                 =  tag "PRE"
sample              =  tag "SAMP"
select              =  tag "SELECT"
small               =  tag "SMALL"
strong              =  tag "STRONG"
style               =  tag "STYLE"
sub                 =  tag "SUB"
sup                 =  tag "SUP"
table               =  tag "TABLE"
td                  =  tag "TD"
textarea            =  tag "TEXTAREA"
th                  =  tag "TH"
thebase             = itag "BASE"
thecode             =  tag "CODE"
thediv              =  tag "DIV"
thehtml             =  tag "HTML"
thelink             =  tag "LINK"
themap              =  tag "MAP"
thespan             =  tag "SPAN"
thetitle            =  tag "TITLE"
tr                  =  tag "TR"
tt                  =  tag "TT"
ulist               =  tag "UL"
underline           =  tag "U"
variable            =  tag "VAR"

-- ---------------------------------------------------------------------------
-- Html Attributes

-- (automatically generated)

action              :: String -> HtmlAttr
align               :: String -> HtmlAttr
alink               :: String -> HtmlAttr
alt                 :: String -> HtmlAttr
altcode             :: String -> HtmlAttr
archive             :: String -> HtmlAttr
background          :: String -> HtmlAttr
base                :: String -> HtmlAttr
bgcolor             :: String -> HtmlAttr
border              :: Int    -> HtmlAttr
bordercolor         :: String -> HtmlAttr
cellpadding         :: Int    -> HtmlAttr
cellspacing         :: Int    -> HtmlAttr
checked             ::           HtmlAttr
clear               :: String -> HtmlAttr
code                :: String -> HtmlAttr
codebase            :: String -> HtmlAttr
color               :: String -> HtmlAttr
cols                :: String -> HtmlAttr
colspan             :: Int    -> HtmlAttr
compact             ::           HtmlAttr
content             :: String -> HtmlAttr
coords              :: String -> HtmlAttr
enctype             :: String -> HtmlAttr
face                :: String -> HtmlAttr
frameborder         :: Int    -> HtmlAttr
height              :: Int    -> HtmlAttr
href                :: String -> HtmlAttr
hspace              :: Int    -> HtmlAttr
httpequiv           :: String -> HtmlAttr
identifier          :: String -> HtmlAttr
ismap               ::           HtmlAttr
lang                :: String -> HtmlAttr
link                :: String -> HtmlAttr
marginheight        :: Int    -> HtmlAttr
marginwidth         :: Int    -> HtmlAttr
maxlength           :: Int    -> HtmlAttr
method              :: String -> HtmlAttr
multiple            ::           HtmlAttr
name                :: String -> HtmlAttr
nohref              ::           HtmlAttr
noresize            ::           HtmlAttr
noshade             ::           HtmlAttr
nowrap              ::           HtmlAttr
rel                 :: String -> HtmlAttr
rev                 :: String -> HtmlAttr
rows                :: String -> HtmlAttr
rowspan             :: Int    -> HtmlAttr
rules               :: String -> HtmlAttr
scrolling           :: String -> HtmlAttr
selected            ::           HtmlAttr
shape               :: String -> HtmlAttr
size                :: String -> HtmlAttr
src                 :: String -> HtmlAttr
start               :: Int    -> HtmlAttr
target              :: String -> HtmlAttr
text                :: String -> HtmlAttr
theclass            :: String -> HtmlAttr
thestyle            :: String -> HtmlAttr
thetype             :: String -> HtmlAttr
title               :: String -> HtmlAttr
usemap              :: String -> HtmlAttr
valign              :: String -> HtmlAttr
value               :: String -> HtmlAttr
version             :: String -> HtmlAttr
vlink               :: String -> HtmlAttr
vspace              :: Int    -> HtmlAttr
width               :: String -> HtmlAttr

action              =   strAttr "ACTION"
align               =   strAttr "ALIGN"
alink               =   strAttr "ALINK"
alt                 =   strAttr "ALT"
altcode             =   strAttr "ALTCODE"
archive             =   strAttr "ARCHIVE"
background          =   strAttr "BACKGROUND"
base                =   strAttr "BASE"
bgcolor             =   strAttr "BGCOLOR"
border              =   intAttr "BORDER"
bordercolor         =   strAttr "BORDERCOLOR"
cellpadding         =   intAttr "CELLPADDING"
cellspacing         =   intAttr "CELLSPACING"
checked             = emptyAttr "CHECKED"
clear               =   strAttr "CLEAR"
code                =   strAttr "CODE"
codebase            =   strAttr "CODEBASE"
color               =   strAttr "COLOR"
cols                =   strAttr "COLS"
colspan             =   intAttr "COLSPAN"
compact             = emptyAttr "COMPACT"
content             =   strAttr "CONTENT"
coords              =   strAttr "COORDS"
enctype             =   strAttr "ENCTYPE"
face                =   strAttr "FACE"
frameborder         =   intAttr "FRAMEBORDER"
height              =   intAttr "HEIGHT"
href                =   strAttr "HREF"
hspace              =   intAttr "HSPACE"
httpequiv           =   strAttr "HTTP-EQUIV"
identifier          =   strAttr "ID"
ismap               = emptyAttr "ISMAP"
lang                =   strAttr "LANG"
link                =   strAttr "LINK"
marginheight        =   intAttr "MARGINHEIGHT"
marginwidth         =   intAttr "MARGINWIDTH"
maxlength           =   intAttr "MAXLENGTH"
method              =   strAttr "METHOD"
multiple            = emptyAttr "MULTIPLE"
name                =   strAttr "NAME"
nohref              = emptyAttr "NOHREF"
noresize            = emptyAttr "NORESIZE"
noshade             = emptyAttr "NOSHADE"
nowrap              = emptyAttr "NOWRAP"
rel                 =   strAttr "REL"
rev                 =   strAttr "REV"
rows                =   strAttr "ROWS"
rowspan             =   intAttr "ROWSPAN"
rules               =   strAttr "RULES"
scrolling           =   strAttr "SCROLLING"
selected            = emptyAttr "SELECTED"
shape               =   strAttr "SHAPE"
size                =   strAttr "SIZE"
src                 =   strAttr "SRC"
start               =   intAttr "START"
target              =   strAttr "TARGET"
text                =   strAttr "TEXT"
theclass            =   strAttr "CLASS"
thestyle            =   strAttr "STYLE"
thetype             =   strAttr "TYPE"
title               =   strAttr "TITLE"
usemap              =   strAttr "USEMAP"
valign              =   strAttr "VALIGN"
value               =   strAttr "VALUE"
version             =   strAttr "VERSION"
vlink               =   strAttr "VLINK"
vspace              =   intAttr "VSPACE"
width               =   strAttr "WIDTH"

-- ---------------------------------------------------------------------------
-- Html Constructors

-- (automatically generated)

validHtmlTags :: [String]
validHtmlTags = [
      "ADDRESS",
      "A",
      "APPLET",
      "BIG",
      "BLOCKQUOTE",
      "BODY",
      "B",
      "CAPTION",
      "CENTER",
      "CITE",
      "DD",
      "DFN",
      "DL",
      "DT",
      "EM",
      "FIELDSET",
      "FONT",
      "FORM",
      "FRAME",
      "FRAMESET",
      "H1",
      "H2",
      "H3",
      "H4",
      "H5",
      "H6",
      "HEAD",
      "I",
      "KBD",
      "LEGEND",
      "LI",
      "NOFRAMES",
      "OL",
      "OPTION",
      "P",
      "PRE",
      "SAMP",
      "SELECT",
      "SMALL",
      "STRONG",
      "STYLE",
      "SUB",
      "SUP",
      "TABLE",
      "TD",
      "TEXTAREA",
      "TH",
      "CODE",
      "DIV",
      "HTML",
      "LINK",
      "MAP",
      "TITLE",
      "TR",
      "TT",
      "UL",
      "U",
      "VAR"]

validHtmlITags :: [String]
validHtmlITags = [
      "AREA",
      "BASEFONT",
      "BR",
      "HR",
      "IMG",
      "INPUT",
      "META",
      "PARAM",
      "BASE"]

validHtmlAttrs :: [String]
validHtmlAttrs = [
      "ACTION",
      "ALIGN",
      "ALINK",
      "ALT",
      "ALTCODE",
      "ARCHIVE",
      "BACKGROUND",
      "BASE",
      "BGCOLOR",
      "BORDER",
      "BORDERCOLOR",
      "CELLPADDING",
      "CELLSPACING",
      "CHECKED",
      "CLEAR",
      "CODE",
      "CODEBASE",
      "COLOR",
      "COLS",
      "COLSPAN",
      "COMPACT",
      "CONTENT",
      "COORDS",
      "ENCTYPE",
      "FACE",
      "FRAMEBORDER",
      "HEIGHT",
      "HREF",
      "HSPACE",
      "HTTP-EQUIV",
      "ID",
      "ISMAP",
      "LANG",
      "LINK",
      "MARGINHEIGHT",
      "MARGINWIDTH",
      "MAXLENGTH",
      "METHOD",
      "MULTIPLE",
      "NAME",
      "NOHREF",
      "NORESIZE",
      "NOSHADE",
      "NOWRAP",
      "REL",
      "REV",
      "ROWS",
      "ROWSPAN",
      "RULES",
      "SCROLLING",
      "SELECTED",
      "SHAPE",
      "SIZE",
      "SRC",
      "START",
      "TARGET",
      "TEXT",
      "CLASS",
      "STYLE",
      "TYPE",
      "TITLE",
      "USEMAP",
      "VALIGN",
      "VALUE",
      "VERSION",
      "VLINK",
      "VSPACE",
      "WIDTH"]

-- ---------------------------------------------------------------------------
-- Html colors

aqua          :: String
black         :: String
blue          :: String
fuchsia       :: String
gray          :: String
green         :: String
lime          :: String
maroon        :: String
navy          :: String
olive         :: String
purple        :: String
red           :: String
silver        :: String
teal          :: String
yellow        :: String
white         :: String

aqua          = "aqua"
black         = "black"
blue          = "blue"
fuchsia       = "fuchsia"
gray          = "gray"
green         = "green"
lime          = "lime"
maroon        = "maroon"
navy          = "navy"
olive         = "olive"
purple        = "purple"
red           = "red"
silver        = "silver"
teal          = "teal"
yellow        = "yellow"
white         = "white"

-- ---------------------------------------------------------------------------
-- Basic Combinators

linesToHtml :: (Html h)=> [String] -> h

linesToHtml []     = noHtml
linesToHtml (x:[]) = lineToHtml x
linesToHtml (x:xs) = lineToHtml x `mappend` br `mappend` linesToHtml xs


-- ---------------------------------------------------------------------------
-- Html abbriviations

primHtmlChar  :: (Html h)=> String -> h
copyright     :: (Html h)=> h
spaceHtml     :: (Html h)=> h
bullet        :: (Html h)=> h
p             :: (Html h)=> h -> h

primHtmlChar  = \ x -> primHtml ("&" ++ x ++ ";")
copyright     = primHtmlChar "copy"
spaceHtml     = primHtmlChar "nbsp"
bullet        = primHtmlChar "#149"

p             = paragraph


