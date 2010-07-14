{-# LANGUAGE OverloadedStrings #-}
-- | This module exports combinators that provide you with the
-- ability to set attributes on HTML elements.
--
module Text.Blaze.Html5.Attributes
    ( accept
    , accept_charset
    , accesskey
    , action
    , alt
    , async
    , autocomplete
    , autofocus
    , autoplay
    , challenge
    , charset
    , checked
    , cite
    , class_
    , cols
    , colspan
    , content
    , contenteditable
    , contextmenu
    , controls
    , coords
    , data_
    , datetime
    , defer
    , dir
    , disabled
    , draggable
    , enctype
    , for
    , form
    , formaction
    , formenctype
    , formmethod
    , formnovalidate
    , formtarget
    , headers
    , height
    , hidden
    , high
    , href
    , hreflang
    , http_equiv
    , icon
    , id
    , ismap
    , item
    , itemprop
    , keytype
    , label
    , lang
    , list
    , loop
    , low
    , manifest
    , max
    , maxlength
    , media
    , method
    , min
    , multiple
    , name
    , novalidate
    , onafterprint
    , onbeforeonload
    , onbeforeprint
    , onblur
    , onerror
    , onfocus
    , onhaschange
    , onload
    , onmessage
    , onoffline
    , ononline
    , onpagehide
    , onpageshow
    , onpropstate
    , onredo
    , onresize
    , onstorage
    , onundo
    , onunload
    , open
    , optimum
    , pattern
    , ping
    , placeholder
    , preload
    , pubdate
    , radiogroup
    , readonly
    , rel
    , required
    , reversed
    , rows
    , rowspan
    , sandbox
    , scope
    , scoped
    , seamless
    , selected
    , shape
    , size
    , sizes
    , span
    , spellcheck
    , src
    , srcdoc
    , start
    , step
    , style
    , subject
    , summary
    , tabindex
    , target
    , title
    , type_
    , usemap
    , value
    , width
    , wrap
    , xmlns
    ) where

import Prelude ()

import Data.Text (Text)

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

-- | Combinator for the @accept@ attribute.
--
-- Example:
--
-- > div ! accept "bar" $ "Hello."
--
-- Result:
--
-- > <div accept="bar">Hello.</div>
--
accept :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
accept = attribute " accept=\""
{-# INLINE accept #-}

-- | Combinator for the @accept-charset@ attribute.
--
-- Example:
--
-- > div ! accept_charset "bar" $ "Hello."
--
-- Result:
--
-- > <div accept-charset="bar">Hello.</div>
--
accept_charset :: AttributeValue  -- ^ Attribute value.
               -> Attribute       -- ^ Resulting attribute.
accept_charset = attribute " accept-charset=\""
{-# INLINE accept_charset #-}

-- | Combinator for the @accesskey@ attribute.
--
-- Example:
--
-- > div ! accesskey "bar" $ "Hello."
--
-- Result:
--
-- > <div accesskey="bar">Hello.</div>
--
accesskey :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
accesskey = attribute " accesskey=\""
{-# INLINE accesskey #-}

-- | Combinator for the @action@ attribute.
--
-- Example:
--
-- > div ! action "bar" $ "Hello."
--
-- Result:
--
-- > <div action="bar">Hello.</div>
--
action :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
action = attribute " action=\""
{-# INLINE action #-}

-- | Combinator for the @alt@ attribute.
--
-- Example:
--
-- > div ! alt "bar" $ "Hello."
--
-- Result:
--
-- > <div alt="bar">Hello.</div>
--
alt :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
alt = attribute " alt=\""
{-# INLINE alt #-}

-- | Combinator for the @async@ attribute.
--
-- Example:
--
-- > div ! async "bar" $ "Hello."
--
-- Result:
--
-- > <div async="bar">Hello.</div>
--
async :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
async = attribute " async=\""
{-# INLINE async #-}

-- | Combinator for the @autocomplete@ attribute.
--
-- Example:
--
-- > div ! autocomplete "bar" $ "Hello."
--
-- Result:
--
-- > <div autocomplete="bar">Hello.</div>
--
autocomplete :: AttributeValue  -- ^ Attribute value.
             -> Attribute       -- ^ Resulting attribute.
autocomplete = attribute " autocomplete=\""
{-# INLINE autocomplete #-}

-- | Combinator for the @autofocus@ attribute.
--
-- Example:
--
-- > div ! autofocus "bar" $ "Hello."
--
-- Result:
--
-- > <div autofocus="bar">Hello.</div>
--
autofocus :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
autofocus = attribute " autofocus=\""
{-# INLINE autofocus #-}

-- | Combinator for the @autoplay@ attribute.
--
-- Example:
--
-- > div ! autoplay "bar" $ "Hello."
--
-- Result:
--
-- > <div autoplay="bar">Hello.</div>
--
autoplay :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
autoplay = attribute " autoplay=\""
{-# INLINE autoplay #-}

-- | Combinator for the @challenge@ attribute.
--
-- Example:
--
-- > div ! challenge "bar" $ "Hello."
--
-- Result:
--
-- > <div challenge="bar">Hello.</div>
--
challenge :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
challenge = attribute " challenge=\""
{-# INLINE challenge #-}

-- | Combinator for the @charset@ attribute.
--
-- Example:
--
-- > div ! charset "bar" $ "Hello."
--
-- Result:
--
-- > <div charset="bar">Hello.</div>
--
charset :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
charset = attribute " charset=\""
{-# INLINE charset #-}

-- | Combinator for the @checked@ attribute.
--
-- Example:
--
-- > div ! checked "bar" $ "Hello."
--
-- Result:
--
-- > <div checked="bar">Hello.</div>
--
checked :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
checked = attribute " checked=\""
{-# INLINE checked #-}

-- | Combinator for the @cite@ attribute.
--
-- Example:
--
-- > div ! cite "bar" $ "Hello."
--
-- Result:
--
-- > <div cite="bar">Hello.</div>
--
cite :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
cite = attribute " cite=\""
{-# INLINE cite #-}

-- | Combinator for the @class@ attribute.
--
-- Example:
--
-- > div ! class_ "bar" $ "Hello."
--
-- Result:
--
-- > <div class="bar">Hello.</div>
--
class_ :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
class_ = attribute " class=\""
{-# INLINE class_ #-}

-- | Combinator for the @cols@ attribute.
--
-- Example:
--
-- > div ! cols "bar" $ "Hello."
--
-- Result:
--
-- > <div cols="bar">Hello.</div>
--
cols :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
cols = attribute " cols=\""
{-# INLINE cols #-}

-- | Combinator for the @colspan@ attribute.
--
-- Example:
--
-- > div ! colspan "bar" $ "Hello."
--
-- Result:
--
-- > <div colspan="bar">Hello.</div>
--
colspan :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
colspan = attribute " colspan=\""
{-# INLINE colspan #-}

-- | Combinator for the @content@ attribute.
--
-- Example:
--
-- > div ! content "bar" $ "Hello."
--
-- Result:
--
-- > <div content="bar">Hello.</div>
--
content :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
content = attribute " content=\""
{-# INLINE content #-}

-- | Combinator for the @contenteditable@ attribute.
--
-- Example:
--
-- > div ! contenteditable "bar" $ "Hello."
--
-- Result:
--
-- > <div contenteditable="bar">Hello.</div>
--
contenteditable :: AttributeValue  -- ^ Attribute value.
                -> Attribute       -- ^ Resulting attribute.
contenteditable = attribute " contenteditable=\""
{-# INLINE contenteditable #-}

-- | Combinator for the @contextmenu@ attribute.
--
-- Example:
--
-- > div ! contextmenu "bar" $ "Hello."
--
-- Result:
--
-- > <div contextmenu="bar">Hello.</div>
--
contextmenu :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
contextmenu = attribute " contextmenu=\""
{-# INLINE contextmenu #-}

-- | Combinator for the @controls@ attribute.
--
-- Example:
--
-- > div ! controls "bar" $ "Hello."
--
-- Result:
--
-- > <div controls="bar">Hello.</div>
--
controls :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
controls = attribute " controls=\""
{-# INLINE controls #-}

-- | Combinator for the @coords@ attribute.
--
-- Example:
--
-- > div ! coords "bar" $ "Hello."
--
-- Result:
--
-- > <div coords="bar">Hello.</div>
--
coords :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
coords = attribute " coords=\""
{-# INLINE coords #-}

-- | Combinator for the @data@ attribute.
--
-- Example:
--
-- > div ! data_ "bar" $ "Hello."
--
-- Result:
--
-- > <div data="bar">Hello.</div>
--
data_ :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
data_ = attribute " data=\""
{-# INLINE data_ #-}

-- | Combinator for the @datetime@ attribute.
--
-- Example:
--
-- > div ! datetime "bar" $ "Hello."
--
-- Result:
--
-- > <div datetime="bar">Hello.</div>
--
datetime :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
datetime = attribute " datetime=\""
{-# INLINE datetime #-}

-- | Combinator for the @defer@ attribute.
--
-- Example:
--
-- > div ! defer "bar" $ "Hello."
--
-- Result:
--
-- > <div defer="bar">Hello.</div>
--
defer :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
defer = attribute " defer=\""
{-# INLINE defer #-}

-- | Combinator for the @dir@ attribute.
--
-- Example:
--
-- > div ! dir "bar" $ "Hello."
--
-- Result:
--
-- > <div dir="bar">Hello.</div>
--
dir :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
dir = attribute " dir=\""
{-# INLINE dir #-}

-- | Combinator for the @disabled@ attribute.
--
-- Example:
--
-- > div ! disabled "bar" $ "Hello."
--
-- Result:
--
-- > <div disabled="bar">Hello.</div>
--
disabled :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
disabled = attribute " disabled=\""
{-# INLINE disabled #-}

-- | Combinator for the @draggable@ attribute.
--
-- Example:
--
-- > div ! draggable "bar" $ "Hello."
--
-- Result:
--
-- > <div draggable="bar">Hello.</div>
--
draggable :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
draggable = attribute " draggable=\""
{-# INLINE draggable #-}

-- | Combinator for the @enctype@ attribute.
--
-- Example:
--
-- > div ! enctype "bar" $ "Hello."
--
-- Result:
--
-- > <div enctype="bar">Hello.</div>
--
enctype :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
enctype = attribute " enctype=\""
{-# INLINE enctype #-}

-- | Combinator for the @for@ attribute.
--
-- Example:
--
-- > div ! for "bar" $ "Hello."
--
-- Result:
--
-- > <div for="bar">Hello.</div>
--
for :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
for = attribute " for=\""
{-# INLINE for #-}

-- | Combinator for the @form@ attribute.
--
-- Example:
--
-- > div ! form "bar" $ "Hello."
--
-- Result:
--
-- > <div form="bar">Hello.</div>
--
form :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
form = attribute " form=\""
{-# INLINE form #-}

-- | Combinator for the @formaction@ attribute.
--
-- Example:
--
-- > div ! formaction "bar" $ "Hello."
--
-- Result:
--
-- > <div formaction="bar">Hello.</div>
--
formaction :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
formaction = attribute " formaction=\""
{-# INLINE formaction #-}

-- | Combinator for the @formenctype@ attribute.
--
-- Example:
--
-- > div ! formenctype "bar" $ "Hello."
--
-- Result:
--
-- > <div formenctype="bar">Hello.</div>
--
formenctype :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
formenctype = attribute " formenctype=\""
{-# INLINE formenctype #-}

-- | Combinator for the @formmethod@ attribute.
--
-- Example:
--
-- > div ! formmethod "bar" $ "Hello."
--
-- Result:
--
-- > <div formmethod="bar">Hello.</div>
--
formmethod :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
formmethod = attribute " formmethod=\""
{-# INLINE formmethod #-}

-- | Combinator for the @formnovalidate@ attribute.
--
-- Example:
--
-- > div ! formnovalidate "bar" $ "Hello."
--
-- Result:
--
-- > <div formnovalidate="bar">Hello.</div>
--
formnovalidate :: AttributeValue  -- ^ Attribute value.
               -> Attribute       -- ^ Resulting attribute.
formnovalidate = attribute " formnovalidate=\""
{-# INLINE formnovalidate #-}

-- | Combinator for the @formtarget@ attribute.
--
-- Example:
--
-- > div ! formtarget "bar" $ "Hello."
--
-- Result:
--
-- > <div formtarget="bar">Hello.</div>
--
formtarget :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
formtarget = attribute " formtarget=\""
{-# INLINE formtarget #-}

-- | Combinator for the @headers@ attribute.
--
-- Example:
--
-- > div ! headers "bar" $ "Hello."
--
-- Result:
--
-- > <div headers="bar">Hello.</div>
--
headers :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
headers = attribute " headers=\""
{-# INLINE headers #-}

-- | Combinator for the @height@ attribute.
--
-- Example:
--
-- > div ! height "bar" $ "Hello."
--
-- Result:
--
-- > <div height="bar">Hello.</div>
--
height :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
height = attribute " height=\""
{-# INLINE height #-}

-- | Combinator for the @hidden@ attribute.
--
-- Example:
--
-- > div ! hidden "bar" $ "Hello."
--
-- Result:
--
-- > <div hidden="bar">Hello.</div>
--
hidden :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
hidden = attribute " hidden=\""
{-# INLINE hidden #-}

-- | Combinator for the @high@ attribute.
--
-- Example:
--
-- > div ! high "bar" $ "Hello."
--
-- Result:
--
-- > <div high="bar">Hello.</div>
--
high :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
high = attribute " high=\""
{-# INLINE high #-}

-- | Combinator for the @href@ attribute.
--
-- Example:
--
-- > div ! href "bar" $ "Hello."
--
-- Result:
--
-- > <div href="bar">Hello.</div>
--
href :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
href = attribute " href=\""
{-# INLINE href #-}

-- | Combinator for the @hreflang@ attribute.
--
-- Example:
--
-- > div ! hreflang "bar" $ "Hello."
--
-- Result:
--
-- > <div hreflang="bar">Hello.</div>
--
hreflang :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
hreflang = attribute " hreflang=\""
{-# INLINE hreflang #-}

-- | Combinator for the @http-equiv@ attribute.
--
-- Example:
--
-- > div ! http_equiv "bar" $ "Hello."
--
-- Result:
--
-- > <div http-equiv="bar">Hello.</div>
--
http_equiv :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
http_equiv = attribute " http-equiv=\""
{-# INLINE http_equiv #-}

-- | Combinator for the @icon@ attribute.
--
-- Example:
--
-- > div ! icon "bar" $ "Hello."
--
-- Result:
--
-- > <div icon="bar">Hello.</div>
--
icon :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
icon = attribute " icon=\""
{-# INLINE icon #-}

-- | Combinator for the @id@ attribute.
--
-- Example:
--
-- > div ! id "bar" $ "Hello."
--
-- Result:
--
-- > <div id="bar">Hello.</div>
--
id :: AttributeValue  -- ^ Attribute value.
   -> Attribute       -- ^ Resulting attribute.
id = attribute " id=\""
{-# INLINE id #-}

-- | Combinator for the @ismap@ attribute.
--
-- Example:
--
-- > div ! ismap "bar" $ "Hello."
--
-- Result:
--
-- > <div ismap="bar">Hello.</div>
--
ismap :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
ismap = attribute " ismap=\""
{-# INLINE ismap #-}

-- | Combinator for the @item@ attribute.
--
-- Example:
--
-- > div ! item "bar" $ "Hello."
--
-- Result:
--
-- > <div item="bar">Hello.</div>
--
item :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
item = attribute " item=\""
{-# INLINE item #-}

-- | Combinator for the @itemprop@ attribute.
--
-- Example:
--
-- > div ! itemprop "bar" $ "Hello."
--
-- Result:
--
-- > <div itemprop="bar">Hello.</div>
--
itemprop :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
itemprop = attribute " itemprop=\""
{-# INLINE itemprop #-}

-- | Combinator for the @keytype@ attribute.
--
-- Example:
--
-- > div ! keytype "bar" $ "Hello."
--
-- Result:
--
-- > <div keytype="bar">Hello.</div>
--
keytype :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
keytype = attribute " keytype=\""
{-# INLINE keytype #-}

-- | Combinator for the @label@ attribute.
--
-- Example:
--
-- > div ! label "bar" $ "Hello."
--
-- Result:
--
-- > <div label="bar">Hello.</div>
--
label :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
label = attribute " label=\""
{-# INLINE label #-}

-- | Combinator for the @lang@ attribute.
--
-- Example:
--
-- > div ! lang "bar" $ "Hello."
--
-- Result:
--
-- > <div lang="bar">Hello.</div>
--
lang :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
lang = attribute " lang=\""
{-# INLINE lang #-}

-- | Combinator for the @list@ attribute.
--
-- Example:
--
-- > div ! list "bar" $ "Hello."
--
-- Result:
--
-- > <div list="bar">Hello.</div>
--
list :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
list = attribute " list=\""
{-# INLINE list #-}

-- | Combinator for the @loop@ attribute.
--
-- Example:
--
-- > div ! loop "bar" $ "Hello."
--
-- Result:
--
-- > <div loop="bar">Hello.</div>
--
loop :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
loop = attribute " loop=\""
{-# INLINE loop #-}

-- | Combinator for the @low@ attribute.
--
-- Example:
--
-- > div ! low "bar" $ "Hello."
--
-- Result:
--
-- > <div low="bar">Hello.</div>
--
low :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
low = attribute " low=\""
{-# INLINE low #-}

-- | Combinator for the @manifest@ attribute.
--
-- Example:
--
-- > div ! manifest "bar" $ "Hello."
--
-- Result:
--
-- > <div manifest="bar">Hello.</div>
--
manifest :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
manifest = attribute " manifest=\""
{-# INLINE manifest #-}

-- | Combinator for the @max@ attribute.
--
-- Example:
--
-- > div ! max "bar" $ "Hello."
--
-- Result:
--
-- > <div max="bar">Hello.</div>
--
max :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
max = attribute " max=\""
{-# INLINE max #-}

-- | Combinator for the @maxlength@ attribute.
--
-- Example:
--
-- > div ! maxlength "bar" $ "Hello."
--
-- Result:
--
-- > <div maxlength="bar">Hello.</div>
--
maxlength :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
maxlength = attribute " maxlength=\""
{-# INLINE maxlength #-}

-- | Combinator for the @media@ attribute.
--
-- Example:
--
-- > div ! media "bar" $ "Hello."
--
-- Result:
--
-- > <div media="bar">Hello.</div>
--
media :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
media = attribute " media=\""
{-# INLINE media #-}

-- | Combinator for the @method@ attribute.
--
-- Example:
--
-- > div ! method "bar" $ "Hello."
--
-- Result:
--
-- > <div method="bar">Hello.</div>
--
method :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
method = attribute " method=\""
{-# INLINE method #-}

-- | Combinator for the @min@ attribute.
--
-- Example:
--
-- > div ! min "bar" $ "Hello."
--
-- Result:
--
-- > <div min="bar">Hello.</div>
--
min :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
min = attribute " min=\""
{-# INLINE min #-}

-- | Combinator for the @multiple@ attribute.
--
-- Example:
--
-- > div ! multiple "bar" $ "Hello."
--
-- Result:
--
-- > <div multiple="bar">Hello.</div>
--
multiple :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
multiple = attribute " multiple=\""
{-# INLINE multiple #-}

-- | Combinator for the @name@ attribute.
--
-- Example:
--
-- > div ! name "bar" $ "Hello."
--
-- Result:
--
-- > <div name="bar">Hello.</div>
--
name :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
name = attribute " name=\""
{-# INLINE name #-}

-- | Combinator for the @novalidate@ attribute.
--
-- Example:
--
-- > div ! novalidate "bar" $ "Hello."
--
-- Result:
--
-- > <div novalidate="bar">Hello.</div>
--
novalidate :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
novalidate = attribute " novalidate=\""
{-# INLINE novalidate #-}

-- | Combinator for the @onafterprint@ attribute.
--
-- Example:
--
-- > div ! onafterprint "bar" $ "Hello."
--
-- Result:
--
-- > <div onafterprint="bar">Hello.</div>
--
onafterprint :: AttributeValue  -- ^ Attribute value.
             -> Attribute       -- ^ Resulting attribute.
onafterprint = attribute " onafterprint=\""
{-# INLINE onafterprint #-}

-- | Combinator for the @onbeforeonload@ attribute.
--
-- Example:
--
-- > div ! onbeforeonload "bar" $ "Hello."
--
-- Result:
--
-- > <div onbeforeonload="bar">Hello.</div>
--
onbeforeonload :: AttributeValue  -- ^ Attribute value.
               -> Attribute       -- ^ Resulting attribute.
onbeforeonload = attribute " onbeforeonload=\""
{-# INLINE onbeforeonload #-}

-- | Combinator for the @onbeforeprint@ attribute.
--
-- Example:
--
-- > div ! onbeforeprint "bar" $ "Hello."
--
-- Result:
--
-- > <div onbeforeprint="bar">Hello.</div>
--
onbeforeprint :: AttributeValue  -- ^ Attribute value.
              -> Attribute       -- ^ Resulting attribute.
onbeforeprint = attribute " onbeforeprint=\""
{-# INLINE onbeforeprint #-}

-- | Combinator for the @onblur@ attribute.
--
-- Example:
--
-- > div ! onblur "bar" $ "Hello."
--
-- Result:
--
-- > <div onblur="bar">Hello.</div>
--
onblur :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onblur = attribute " onblur=\""
{-# INLINE onblur #-}

-- | Combinator for the @onerror@ attribute.
--
-- Example:
--
-- > div ! onerror "bar" $ "Hello."
--
-- Result:
--
-- > <div onerror="bar">Hello.</div>
--
onerror :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onerror = attribute " onerror=\""
{-# INLINE onerror #-}

-- | Combinator for the @onfocus@ attribute.
--
-- Example:
--
-- > div ! onfocus "bar" $ "Hello."
--
-- Result:
--
-- > <div onfocus="bar">Hello.</div>
--
onfocus :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
onfocus = attribute " onfocus=\""
{-# INLINE onfocus #-}

-- | Combinator for the @onhaschange@ attribute.
--
-- Example:
--
-- > div ! onhaschange "bar" $ "Hello."
--
-- Result:
--
-- > <div onhaschange="bar">Hello.</div>
--
onhaschange :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
onhaschange = attribute " onhaschange=\""
{-# INLINE onhaschange #-}

-- | Combinator for the @onload@ attribute.
--
-- Example:
--
-- > div ! onload "bar" $ "Hello."
--
-- Result:
--
-- > <div onload="bar">Hello.</div>
--
onload :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onload = attribute " onload=\""
{-# INLINE onload #-}

-- | Combinator for the @onmessage@ attribute.
--
-- Example:
--
-- > div ! onmessage "bar" $ "Hello."
--
-- Result:
--
-- > <div onmessage="bar">Hello.</div>
--
onmessage :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
onmessage = attribute " onmessage=\""
{-# INLINE onmessage #-}

-- | Combinator for the @onoffline@ attribute.
--
-- Example:
--
-- > div ! onoffline "bar" $ "Hello."
--
-- Result:
--
-- > <div onoffline="bar">Hello.</div>
--
onoffline :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
onoffline = attribute " onoffline=\""
{-# INLINE onoffline #-}

-- | Combinator for the @ononline@ attribute.
--
-- Example:
--
-- > div ! ononline "bar" $ "Hello."
--
-- Result:
--
-- > <div ononline="bar">Hello.</div>
--
ononline :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
ononline = attribute " ononline=\""
{-# INLINE ononline #-}

-- | Combinator for the @onpagehide@ attribute.
--
-- Example:
--
-- > div ! onpagehide "bar" $ "Hello."
--
-- Result:
--
-- > <div onpagehide="bar">Hello.</div>
--
onpagehide :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
onpagehide = attribute " onpagehide=\""
{-# INLINE onpagehide #-}

-- | Combinator for the @onpageshow@ attribute.
--
-- Example:
--
-- > div ! onpageshow "bar" $ "Hello."
--
-- Result:
--
-- > <div onpageshow="bar">Hello.</div>
--
onpageshow :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
onpageshow = attribute " onpageshow=\""
{-# INLINE onpageshow #-}

-- | Combinator for the @onpropstate@ attribute.
--
-- Example:
--
-- > div ! onpropstate "bar" $ "Hello."
--
-- Result:
--
-- > <div onpropstate="bar">Hello.</div>
--
onpropstate :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
onpropstate = attribute " onpropstate=\""
{-# INLINE onpropstate #-}

-- | Combinator for the @onredo@ attribute.
--
-- Example:
--
-- > div ! onredo "bar" $ "Hello."
--
-- Result:
--
-- > <div onredo="bar">Hello.</div>
--
onredo :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onredo = attribute " onredo=\""
{-# INLINE onredo #-}

-- | Combinator for the @onresize@ attribute.
--
-- Example:
--
-- > div ! onresize "bar" $ "Hello."
--
-- Result:
--
-- > <div onresize="bar">Hello.</div>
--
onresize :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onresize = attribute " onresize=\""
{-# INLINE onresize #-}

-- | Combinator for the @onstorage@ attribute.
--
-- Example:
--
-- > div ! onstorage "bar" $ "Hello."
--
-- Result:
--
-- > <div onstorage="bar">Hello.</div>
--
onstorage :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
onstorage = attribute " onstorage=\""
{-# INLINE onstorage #-}

-- | Combinator for the @onundo@ attribute.
--
-- Example:
--
-- > div ! onundo "bar" $ "Hello."
--
-- Result:
--
-- > <div onundo="bar">Hello.</div>
--
onundo :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
onundo = attribute " onundo=\""
{-# INLINE onundo #-}

-- | Combinator for the @onunload@ attribute.
--
-- Example:
--
-- > div ! onunload "bar" $ "Hello."
--
-- Result:
--
-- > <div onunload="bar">Hello.</div>
--
onunload :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
onunload = attribute " onunload=\""
{-# INLINE onunload #-}

-- | Combinator for the @open@ attribute.
--
-- Example:
--
-- > div ! open "bar" $ "Hello."
--
-- Result:
--
-- > <div open="bar">Hello.</div>
--
open :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
open = attribute " open=\""
{-# INLINE open #-}

-- | Combinator for the @optimum@ attribute.
--
-- Example:
--
-- > div ! optimum "bar" $ "Hello."
--
-- Result:
--
-- > <div optimum="bar">Hello.</div>
--
optimum :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
optimum = attribute " optimum=\""
{-# INLINE optimum #-}

-- | Combinator for the @pattern@ attribute.
--
-- Example:
--
-- > div ! pattern "bar" $ "Hello."
--
-- Result:
--
-- > <div pattern="bar">Hello.</div>
--
pattern :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
pattern = attribute " pattern=\""
{-# INLINE pattern #-}

-- | Combinator for the @ping@ attribute.
--
-- Example:
--
-- > div ! ping "bar" $ "Hello."
--
-- Result:
--
-- > <div ping="bar">Hello.</div>
--
ping :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
ping = attribute " ping=\""
{-# INLINE ping #-}

-- | Combinator for the @placeholder@ attribute.
--
-- Example:
--
-- > div ! placeholder "bar" $ "Hello."
--
-- Result:
--
-- > <div placeholder="bar">Hello.</div>
--
placeholder :: AttributeValue  -- ^ Attribute value.
            -> Attribute       -- ^ Resulting attribute.
placeholder = attribute " placeholder=\""
{-# INLINE placeholder #-}

-- | Combinator for the @preload@ attribute.
--
-- Example:
--
-- > div ! preload "bar" $ "Hello."
--
-- Result:
--
-- > <div preload="bar">Hello.</div>
--
preload :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
preload = attribute " preload=\""
{-# INLINE preload #-}

-- | Combinator for the @pubdate@ attribute.
--
-- Example:
--
-- > div ! pubdate "bar" $ "Hello."
--
-- Result:
--
-- > <div pubdate="bar">Hello.</div>
--
pubdate :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
pubdate = attribute " pubdate=\""
{-# INLINE pubdate #-}

-- | Combinator for the @radiogroup@ attribute.
--
-- Example:
--
-- > div ! radiogroup "bar" $ "Hello."
--
-- Result:
--
-- > <div radiogroup="bar">Hello.</div>
--
radiogroup :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
radiogroup = attribute " radiogroup=\""
{-# INLINE radiogroup #-}

-- | Combinator for the @readonly@ attribute.
--
-- Example:
--
-- > div ! readonly "bar" $ "Hello."
--
-- Result:
--
-- > <div readonly="bar">Hello.</div>
--
readonly :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
readonly = attribute " readonly=\""
{-# INLINE readonly #-}

-- | Combinator for the @rel@ attribute.
--
-- Example:
--
-- > div ! rel "bar" $ "Hello."
--
-- Result:
--
-- > <div rel="bar">Hello.</div>
--
rel :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
rel = attribute " rel=\""
{-# INLINE rel #-}

-- | Combinator for the @required@ attribute.
--
-- Example:
--
-- > div ! required "bar" $ "Hello."
--
-- Result:
--
-- > <div required="bar">Hello.</div>
--
required :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
required = attribute " required=\""
{-# INLINE required #-}

-- | Combinator for the @reversed@ attribute.
--
-- Example:
--
-- > div ! reversed "bar" $ "Hello."
--
-- Result:
--
-- > <div reversed="bar">Hello.</div>
--
reversed :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
reversed = attribute " reversed=\""
{-# INLINE reversed #-}

-- | Combinator for the @rows@ attribute.
--
-- Example:
--
-- > div ! rows "bar" $ "Hello."
--
-- Result:
--
-- > <div rows="bar">Hello.</div>
--
rows :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
rows = attribute " rows=\""
{-# INLINE rows #-}

-- | Combinator for the @rowspan@ attribute.
--
-- Example:
--
-- > div ! rowspan "bar" $ "Hello."
--
-- Result:
--
-- > <div rowspan="bar">Hello.</div>
--
rowspan :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
rowspan = attribute " rowspan=\""
{-# INLINE rowspan #-}

-- | Combinator for the @sandbox@ attribute.
--
-- Example:
--
-- > div ! sandbox "bar" $ "Hello."
--
-- Result:
--
-- > <div sandbox="bar">Hello.</div>
--
sandbox :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
sandbox = attribute " sandbox=\""
{-# INLINE sandbox #-}

-- | Combinator for the @scope@ attribute.
--
-- Example:
--
-- > div ! scope "bar" $ "Hello."
--
-- Result:
--
-- > <div scope="bar">Hello.</div>
--
scope :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
scope = attribute " scope=\""
{-# INLINE scope #-}

-- | Combinator for the @scoped@ attribute.
--
-- Example:
--
-- > div ! scoped "bar" $ "Hello."
--
-- Result:
--
-- > <div scoped="bar">Hello.</div>
--
scoped :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
scoped = attribute " scoped=\""
{-# INLINE scoped #-}

-- | Combinator for the @seamless@ attribute.
--
-- Example:
--
-- > div ! seamless "bar" $ "Hello."
--
-- Result:
--
-- > <div seamless="bar">Hello.</div>
--
seamless :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
seamless = attribute " seamless=\""
{-# INLINE seamless #-}

-- | Combinator for the @selected@ attribute.
--
-- Example:
--
-- > div ! selected "bar" $ "Hello."
--
-- Result:
--
-- > <div selected="bar">Hello.</div>
--
selected :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
selected = attribute " selected=\""
{-# INLINE selected #-}

-- | Combinator for the @shape@ attribute.
--
-- Example:
--
-- > div ! shape "bar" $ "Hello."
--
-- Result:
--
-- > <div shape="bar">Hello.</div>
--
shape :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
shape = attribute " shape=\""
{-# INLINE shape #-}

-- | Combinator for the @size@ attribute.
--
-- Example:
--
-- > div ! size "bar" $ "Hello."
--
-- Result:
--
-- > <div size="bar">Hello.</div>
--
size :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
size = attribute " size=\""
{-# INLINE size #-}

-- | Combinator for the @sizes@ attribute.
--
-- Example:
--
-- > div ! sizes "bar" $ "Hello."
--
-- Result:
--
-- > <div sizes="bar">Hello.</div>
--
sizes :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
sizes = attribute " sizes=\""
{-# INLINE sizes #-}

-- | Combinator for the @span@ attribute.
--
-- Example:
--
-- > div ! span "bar" $ "Hello."
--
-- Result:
--
-- > <div span="bar">Hello.</div>
--
span :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
span = attribute " span=\""
{-# INLINE span #-}

-- | Combinator for the @spellcheck@ attribute.
--
-- Example:
--
-- > div ! spellcheck "bar" $ "Hello."
--
-- Result:
--
-- > <div spellcheck="bar">Hello.</div>
--
spellcheck :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
spellcheck = attribute " spellcheck=\""
{-# INLINE spellcheck #-}

-- | Combinator for the @src@ attribute.
--
-- Example:
--
-- > div ! src "bar" $ "Hello."
--
-- Result:
--
-- > <div src="bar">Hello.</div>
--
src :: AttributeValue  -- ^ Attribute value.
    -> Attribute       -- ^ Resulting attribute.
src = attribute " src=\""
{-# INLINE src #-}

-- | Combinator for the @srcdoc@ attribute.
--
-- Example:
--
-- > div ! srcdoc "bar" $ "Hello."
--
-- Result:
--
-- > <div srcdoc="bar">Hello.</div>
--
srcdoc :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
srcdoc = attribute " srcdoc=\""
{-# INLINE srcdoc #-}

-- | Combinator for the @start@ attribute.
--
-- Example:
--
-- > div ! start "bar" $ "Hello."
--
-- Result:
--
-- > <div start="bar">Hello.</div>
--
start :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
start = attribute " start=\""
{-# INLINE start #-}

-- | Combinator for the @step@ attribute.
--
-- Example:
--
-- > div ! step "bar" $ "Hello."
--
-- Result:
--
-- > <div step="bar">Hello.</div>
--
step :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
step = attribute " step=\""
{-# INLINE step #-}

-- | Combinator for the @style@ attribute.
--
-- Example:
--
-- > div ! style "bar" $ "Hello."
--
-- Result:
--
-- > <div style="bar">Hello.</div>
--
style :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
style = attribute " style=\""
{-# INLINE style #-}

-- | Combinator for the @subject@ attribute.
--
-- Example:
--
-- > div ! subject "bar" $ "Hello."
--
-- Result:
--
-- > <div subject="bar">Hello.</div>
--
subject :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
subject = attribute " subject=\""
{-# INLINE subject #-}

-- | Combinator for the @summary@ attribute.
--
-- Example:
--
-- > div ! summary "bar" $ "Hello."
--
-- Result:
--
-- > <div summary="bar">Hello.</div>
--
summary :: AttributeValue  -- ^ Attribute value.
        -> Attribute       -- ^ Resulting attribute.
summary = attribute " summary=\""
{-# INLINE summary #-}

-- | Combinator for the @tabindex@ attribute.
--
-- Example:
--
-- > div ! tabindex "bar" $ "Hello."
--
-- Result:
--
-- > <div tabindex="bar">Hello.</div>
--
tabindex :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
tabindex = attribute " tabindex=\""
{-# INLINE tabindex #-}

-- | Combinator for the @target@ attribute.
--
-- Example:
--
-- > div ! target "bar" $ "Hello."
--
-- Result:
--
-- > <div target="bar">Hello.</div>
--
target :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
target = attribute " target=\""
{-# INLINE target #-}

-- | Combinator for the @title@ attribute.
--
-- Example:
--
-- > div ! title "bar" $ "Hello."
--
-- Result:
--
-- > <div title="bar">Hello.</div>
--
title :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
title = attribute " title=\""
{-# INLINE title #-}

-- | Combinator for the @type@ attribute.
--
-- Example:
--
-- > div ! type_ "bar" $ "Hello."
--
-- Result:
--
-- > <div type="bar">Hello.</div>
--
type_ :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
type_ = attribute " type=\""
{-# INLINE type_ #-}

-- | Combinator for the @usemap@ attribute.
--
-- Example:
--
-- > div ! usemap "bar" $ "Hello."
--
-- Result:
--
-- > <div usemap="bar">Hello.</div>
--
usemap :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
usemap = attribute " usemap=\""
{-# INLINE usemap #-}

-- | Combinator for the @value@ attribute.
--
-- Example:
--
-- > div ! value "bar" $ "Hello."
--
-- Result:
--
-- > <div value="bar">Hello.</div>
--
value :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
value = attribute " value=\""
{-# INLINE value #-}

-- | Combinator for the @width@ attribute.
--
-- Example:
--
-- > div ! width "bar" $ "Hello."
--
-- Result:
--
-- > <div width="bar">Hello.</div>
--
width :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
width = attribute " width=\""
{-# INLINE width #-}

-- | Combinator for the @wrap@ attribute.
--
-- Example:
--
-- > div ! wrap "bar" $ "Hello."
--
-- Result:
--
-- > <div wrap="bar">Hello.</div>
--
wrap :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
wrap = attribute " wrap=\""
{-# INLINE wrap #-}

-- | Combinator for the @xmlns@ attribute.
--
-- Example:
--
-- > div ! xmlns "bar" $ "Hello."
--
-- Result:
--
-- > <div xmlns="bar">Hello.</div>
--
xmlns :: AttributeValue  -- ^ Attribute value.
      -> Attribute       -- ^ Resulting attribute.
xmlns = attribute " xmlns=\""
{-# INLINE xmlns #-}
