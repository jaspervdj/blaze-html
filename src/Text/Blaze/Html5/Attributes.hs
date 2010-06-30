{-# LANGUAGE OverloadedStrings #-}                                              -- GenerateHtmlVariant.hs:69
-- | This module exports combinators that provide you with the
-- ability to set attributes on HTML elements.
--
module Text.Blaze.Html5.Attributes                                              -- GenerateHtmlVariant.hs:111
    ( accept                                                                    -- GenerateHtmlVariant.hs:112
    , accept_charset                                                            -- GenerateHtmlVariant.hs:114
    , accesskey                                                                 -- GenerateHtmlVariant.hs:114
    , action                                                                    -- GenerateHtmlVariant.hs:114
    , alt                                                                       -- GenerateHtmlVariant.hs:114
    , async                                                                     -- GenerateHtmlVariant.hs:114
    , autocomplete                                                              -- GenerateHtmlVariant.hs:114
    , autofocus                                                                 -- GenerateHtmlVariant.hs:114
    , autoplay                                                                  -- GenerateHtmlVariant.hs:114
    , challenge                                                                 -- GenerateHtmlVariant.hs:114
    , charset                                                                   -- GenerateHtmlVariant.hs:114
    , checked                                                                   -- GenerateHtmlVariant.hs:114
    , cite                                                                      -- GenerateHtmlVariant.hs:114
    , class_                                                                    -- GenerateHtmlVariant.hs:114
    , cols                                                                      -- GenerateHtmlVariant.hs:114
    , colspan                                                                   -- GenerateHtmlVariant.hs:114
    , content                                                                   -- GenerateHtmlVariant.hs:114
    , contenteditable                                                           -- GenerateHtmlVariant.hs:114
    , contextmenu                                                               -- GenerateHtmlVariant.hs:114
    , controls                                                                  -- GenerateHtmlVariant.hs:114
    , coords                                                                    -- GenerateHtmlVariant.hs:114
    , data_                                                                     -- GenerateHtmlVariant.hs:114
    , datetime                                                                  -- GenerateHtmlVariant.hs:114
    , defer                                                                     -- GenerateHtmlVariant.hs:114
    , dir                                                                       -- GenerateHtmlVariant.hs:114
    , disabled                                                                  -- GenerateHtmlVariant.hs:114
    , draggable                                                                 -- GenerateHtmlVariant.hs:114
    , enctype                                                                   -- GenerateHtmlVariant.hs:114
    , for                                                                       -- GenerateHtmlVariant.hs:114
    , form                                                                      -- GenerateHtmlVariant.hs:114
    , formaction                                                                -- GenerateHtmlVariant.hs:114
    , formenctype                                                               -- GenerateHtmlVariant.hs:114
    , formmethod                                                                -- GenerateHtmlVariant.hs:114
    , formnovalidate                                                            -- GenerateHtmlVariant.hs:114
    , formtarget                                                                -- GenerateHtmlVariant.hs:114
    , headers                                                                   -- GenerateHtmlVariant.hs:114
    , height                                                                    -- GenerateHtmlVariant.hs:114
    , hidden                                                                    -- GenerateHtmlVariant.hs:114
    , high                                                                      -- GenerateHtmlVariant.hs:114
    , href                                                                      -- GenerateHtmlVariant.hs:114
    , hreflang                                                                  -- GenerateHtmlVariant.hs:114
    , http_equiv                                                                -- GenerateHtmlVariant.hs:114
    , icon                                                                      -- GenerateHtmlVariant.hs:114
    , id                                                                        -- GenerateHtmlVariant.hs:114
    , ismap                                                                     -- GenerateHtmlVariant.hs:114
    , item                                                                      -- GenerateHtmlVariant.hs:114
    , itemprop                                                                  -- GenerateHtmlVariant.hs:114
    , keytype                                                                   -- GenerateHtmlVariant.hs:114
    , label                                                                     -- GenerateHtmlVariant.hs:114
    , lang                                                                      -- GenerateHtmlVariant.hs:114
    , list                                                                      -- GenerateHtmlVariant.hs:114
    , loop                                                                      -- GenerateHtmlVariant.hs:114
    , low                                                                       -- GenerateHtmlVariant.hs:114
    , manifest                                                                  -- GenerateHtmlVariant.hs:114
    , max                                                                       -- GenerateHtmlVariant.hs:114
    , maxlength                                                                 -- GenerateHtmlVariant.hs:114
    , media                                                                     -- GenerateHtmlVariant.hs:114
    , method                                                                    -- GenerateHtmlVariant.hs:114
    , min                                                                       -- GenerateHtmlVariant.hs:114
    , multiple                                                                  -- GenerateHtmlVariant.hs:114
    , name                                                                      -- GenerateHtmlVariant.hs:114
    , novalidate                                                                -- GenerateHtmlVariant.hs:114
    , onafterprint                                                              -- GenerateHtmlVariant.hs:114
    , onbeforeonload                                                            -- GenerateHtmlVariant.hs:114
    , onbeforeprint                                                             -- GenerateHtmlVariant.hs:114
    , onblur                                                                    -- GenerateHtmlVariant.hs:114
    , onerror                                                                   -- GenerateHtmlVariant.hs:114
    , onfocus                                                                   -- GenerateHtmlVariant.hs:114
    , onhaschange                                                               -- GenerateHtmlVariant.hs:114
    , onload                                                                    -- GenerateHtmlVariant.hs:114
    , onmessage                                                                 -- GenerateHtmlVariant.hs:114
    , onoffline                                                                 -- GenerateHtmlVariant.hs:114
    , ononline                                                                  -- GenerateHtmlVariant.hs:114
    , onpagehide                                                                -- GenerateHtmlVariant.hs:114
    , onpageshow                                                                -- GenerateHtmlVariant.hs:114
    , onpropstate                                                               -- GenerateHtmlVariant.hs:114
    , onredo                                                                    -- GenerateHtmlVariant.hs:114
    , onresize                                                                  -- GenerateHtmlVariant.hs:114
    , onstorage                                                                 -- GenerateHtmlVariant.hs:114
    , onundo                                                                    -- GenerateHtmlVariant.hs:114
    , onunload                                                                  -- GenerateHtmlVariant.hs:114
    , open                                                                      -- GenerateHtmlVariant.hs:114
    , optimum                                                                   -- GenerateHtmlVariant.hs:114
    , pattern                                                                   -- GenerateHtmlVariant.hs:114
    , ping                                                                      -- GenerateHtmlVariant.hs:114
    , placeholder                                                               -- GenerateHtmlVariant.hs:114
    , preload                                                                   -- GenerateHtmlVariant.hs:114
    , pubdate                                                                   -- GenerateHtmlVariant.hs:114
    , radiogroup                                                                -- GenerateHtmlVariant.hs:114
    , readonly                                                                  -- GenerateHtmlVariant.hs:114
    , rel                                                                       -- GenerateHtmlVariant.hs:114
    , required                                                                  -- GenerateHtmlVariant.hs:114
    , reversed                                                                  -- GenerateHtmlVariant.hs:114
    , rows                                                                      -- GenerateHtmlVariant.hs:114
    , rowspan                                                                   -- GenerateHtmlVariant.hs:114
    , sandbox                                                                   -- GenerateHtmlVariant.hs:114
    , scope                                                                     -- GenerateHtmlVariant.hs:114
    , scoped                                                                    -- GenerateHtmlVariant.hs:114
    , seamless                                                                  -- GenerateHtmlVariant.hs:114
    , selected                                                                  -- GenerateHtmlVariant.hs:114
    , shape                                                                     -- GenerateHtmlVariant.hs:114
    , size                                                                      -- GenerateHtmlVariant.hs:114
    , sizes                                                                     -- GenerateHtmlVariant.hs:114
    , span                                                                      -- GenerateHtmlVariant.hs:114
    , spellcheck                                                                -- GenerateHtmlVariant.hs:114
    , src                                                                       -- GenerateHtmlVariant.hs:114
    , srcdoc                                                                    -- GenerateHtmlVariant.hs:114
    , start                                                                     -- GenerateHtmlVariant.hs:114
    , step                                                                      -- GenerateHtmlVariant.hs:114
    , style                                                                     -- GenerateHtmlVariant.hs:114
    , subject                                                                   -- GenerateHtmlVariant.hs:114
    , summary                                                                   -- GenerateHtmlVariant.hs:114
    , tabindex                                                                  -- GenerateHtmlVariant.hs:114
    , target                                                                    -- GenerateHtmlVariant.hs:114
    , title                                                                     -- GenerateHtmlVariant.hs:114
    , type_                                                                     -- GenerateHtmlVariant.hs:114
    , usemap                                                                    -- GenerateHtmlVariant.hs:114
    , value                                                                     -- GenerateHtmlVariant.hs:114
    , width                                                                     -- GenerateHtmlVariant.hs:114
    , wrap                                                                      -- GenerateHtmlVariant.hs:114
    , xmlns                                                                     -- GenerateHtmlVariant.hs:114
    ) where                                                                     -- GenerateHtmlVariant.hs:115

import Prelude ()                                                               -- GenerateHtmlVariant.hs:74
                                                                                -- GenerateHtmlVariant.hs:75
import Data.Text (Text)                                                         -- GenerateHtmlVariant.hs:76
                                                                                -- GenerateHtmlVariant.hs:77
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)               -- GenerateHtmlVariant.hs:78
                                                                                -- GenerateHtmlVariant.hs:79
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
accept :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
accept = attribute " accept=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE accept #-}                                                           -- GenerateHtmlVariant.hs:239

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
accept_charset :: AttributeValue  -- ^ Attribute value.                         -- GenerateHtmlVariant.hs:236
               -> Attribute       -- ^ Resulting attribute.                     -- GenerateHtmlVariant.hs:237
accept_charset = attribute " accept-charset=\""                                 -- GenerateHtmlVariant.hs:238
{-# INLINE accept_charset #-}                                                   -- GenerateHtmlVariant.hs:239

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
accesskey :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
accesskey = attribute " accesskey=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE accesskey #-}                                                        -- GenerateHtmlVariant.hs:239

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
action :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
action = attribute " action=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE action #-}                                                           -- GenerateHtmlVariant.hs:239

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
alt :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
alt = attribute " alt=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE alt #-}                                                              -- GenerateHtmlVariant.hs:239

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
async :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
async = attribute " async=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE async #-}                                                            -- GenerateHtmlVariant.hs:239

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
autocomplete :: AttributeValue  -- ^ Attribute value.                           -- GenerateHtmlVariant.hs:236
             -> Attribute       -- ^ Resulting attribute.                       -- GenerateHtmlVariant.hs:237
autocomplete = attribute " autocomplete=\""                                     -- GenerateHtmlVariant.hs:238
{-# INLINE autocomplete #-}                                                     -- GenerateHtmlVariant.hs:239

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
autofocus :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
autofocus = attribute " autofocus=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE autofocus #-}                                                        -- GenerateHtmlVariant.hs:239

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
autoplay :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
autoplay = attribute " autoplay=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE autoplay #-}                                                         -- GenerateHtmlVariant.hs:239

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
challenge :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
challenge = attribute " challenge=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE challenge #-}                                                        -- GenerateHtmlVariant.hs:239

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
charset :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
charset = attribute " charset=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE charset #-}                                                          -- GenerateHtmlVariant.hs:239

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
checked :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
checked = attribute " checked=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE checked #-}                                                          -- GenerateHtmlVariant.hs:239

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
cite :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
cite = attribute " cite=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE cite #-}                                                             -- GenerateHtmlVariant.hs:239

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
class_ :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
class_ = attribute " class=\""                                                  -- GenerateHtmlVariant.hs:238
{-# INLINE class_ #-}                                                           -- GenerateHtmlVariant.hs:239

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
cols :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
cols = attribute " cols=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE cols #-}                                                             -- GenerateHtmlVariant.hs:239

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
colspan :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
colspan = attribute " colspan=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE colspan #-}                                                          -- GenerateHtmlVariant.hs:239

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
content :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
content = attribute " content=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE content #-}                                                          -- GenerateHtmlVariant.hs:239

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
contenteditable :: AttributeValue  -- ^ Attribute value.                        -- GenerateHtmlVariant.hs:236
                -> Attribute       -- ^ Resulting attribute.                    -- GenerateHtmlVariant.hs:237
contenteditable = attribute " contenteditable=\""                               -- GenerateHtmlVariant.hs:238
{-# INLINE contenteditable #-}                                                  -- GenerateHtmlVariant.hs:239

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
contextmenu :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
contextmenu = attribute " contextmenu=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE contextmenu #-}                                                      -- GenerateHtmlVariant.hs:239

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
controls :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
controls = attribute " controls=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE controls #-}                                                         -- GenerateHtmlVariant.hs:239

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
coords :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
coords = attribute " coords=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE coords #-}                                                           -- GenerateHtmlVariant.hs:239

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
data_ :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
data_ = attribute " data=\""                                                    -- GenerateHtmlVariant.hs:238
{-# INLINE data_ #-}                                                            -- GenerateHtmlVariant.hs:239

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
datetime :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
datetime = attribute " datetime=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE datetime #-}                                                         -- GenerateHtmlVariant.hs:239

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
defer :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
defer = attribute " defer=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE defer #-}                                                            -- GenerateHtmlVariant.hs:239

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
dir :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
dir = attribute " dir=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE dir #-}                                                              -- GenerateHtmlVariant.hs:239

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
disabled :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
disabled = attribute " disabled=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE disabled #-}                                                         -- GenerateHtmlVariant.hs:239

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
draggable :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
draggable = attribute " draggable=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE draggable #-}                                                        -- GenerateHtmlVariant.hs:239

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
enctype :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
enctype = attribute " enctype=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE enctype #-}                                                          -- GenerateHtmlVariant.hs:239

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
for :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
for = attribute " for=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE for #-}                                                              -- GenerateHtmlVariant.hs:239

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
form :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
form = attribute " form=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE form #-}                                                             -- GenerateHtmlVariant.hs:239

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
formaction :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
formaction = attribute " formaction=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE formaction #-}                                                       -- GenerateHtmlVariant.hs:239

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
formenctype :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
formenctype = attribute " formenctype=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE formenctype #-}                                                      -- GenerateHtmlVariant.hs:239

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
formmethod :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
formmethod = attribute " formmethod=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE formmethod #-}                                                       -- GenerateHtmlVariant.hs:239

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
formnovalidate :: AttributeValue  -- ^ Attribute value.                         -- GenerateHtmlVariant.hs:236
               -> Attribute       -- ^ Resulting attribute.                     -- GenerateHtmlVariant.hs:237
formnovalidate = attribute " formnovalidate=\""                                 -- GenerateHtmlVariant.hs:238
{-# INLINE formnovalidate #-}                                                   -- GenerateHtmlVariant.hs:239

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
formtarget :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
formtarget = attribute " formtarget=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE formtarget #-}                                                       -- GenerateHtmlVariant.hs:239

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
headers :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
headers = attribute " headers=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE headers #-}                                                          -- GenerateHtmlVariant.hs:239

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
height :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
height = attribute " height=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE height #-}                                                           -- GenerateHtmlVariant.hs:239

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
hidden :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
hidden = attribute " hidden=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE hidden #-}                                                           -- GenerateHtmlVariant.hs:239

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
high :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
high = attribute " high=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE high #-}                                                             -- GenerateHtmlVariant.hs:239

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
href :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
href = attribute " href=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE href #-}                                                             -- GenerateHtmlVariant.hs:239

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
hreflang :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
hreflang = attribute " hreflang=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE hreflang #-}                                                         -- GenerateHtmlVariant.hs:239

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
http_equiv :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
http_equiv = attribute " http-equiv=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE http_equiv #-}                                                       -- GenerateHtmlVariant.hs:239

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
icon :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
icon = attribute " icon=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE icon #-}                                                             -- GenerateHtmlVariant.hs:239

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
id :: AttributeValue  -- ^ Attribute value.                                     -- GenerateHtmlVariant.hs:236
   -> Attribute       -- ^ Resulting attribute.                                 -- GenerateHtmlVariant.hs:237
id = attribute " id=\""                                                         -- GenerateHtmlVariant.hs:238
{-# INLINE id #-}                                                               -- GenerateHtmlVariant.hs:239

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
ismap :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
ismap = attribute " ismap=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE ismap #-}                                                            -- GenerateHtmlVariant.hs:239

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
item :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
item = attribute " item=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE item #-}                                                             -- GenerateHtmlVariant.hs:239

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
itemprop :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
itemprop = attribute " itemprop=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE itemprop #-}                                                         -- GenerateHtmlVariant.hs:239

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
keytype :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
keytype = attribute " keytype=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE keytype #-}                                                          -- GenerateHtmlVariant.hs:239

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
label :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
label = attribute " label=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE label #-}                                                            -- GenerateHtmlVariant.hs:239

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
lang :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
lang = attribute " lang=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE lang #-}                                                             -- GenerateHtmlVariant.hs:239

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
list :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
list = attribute " list=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE list #-}                                                             -- GenerateHtmlVariant.hs:239

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
loop :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
loop = attribute " loop=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE loop #-}                                                             -- GenerateHtmlVariant.hs:239

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
low :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
low = attribute " low=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE low #-}                                                              -- GenerateHtmlVariant.hs:239

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
manifest :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
manifest = attribute " manifest=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE manifest #-}                                                         -- GenerateHtmlVariant.hs:239

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
max :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
max = attribute " max=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE max #-}                                                              -- GenerateHtmlVariant.hs:239

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
maxlength :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
maxlength = attribute " maxlength=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE maxlength #-}                                                        -- GenerateHtmlVariant.hs:239

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
media :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
media = attribute " media=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE media #-}                                                            -- GenerateHtmlVariant.hs:239

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
method :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
method = attribute " method=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE method #-}                                                           -- GenerateHtmlVariant.hs:239

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
min :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
min = attribute " min=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE min #-}                                                              -- GenerateHtmlVariant.hs:239

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
multiple :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
multiple = attribute " multiple=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE multiple #-}                                                         -- GenerateHtmlVariant.hs:239

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
name :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
name = attribute " name=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE name #-}                                                             -- GenerateHtmlVariant.hs:239

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
novalidate :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
novalidate = attribute " novalidate=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE novalidate #-}                                                       -- GenerateHtmlVariant.hs:239

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
onafterprint :: AttributeValue  -- ^ Attribute value.                           -- GenerateHtmlVariant.hs:236
             -> Attribute       -- ^ Resulting attribute.                       -- GenerateHtmlVariant.hs:237
onafterprint = attribute " onafterprint=\""                                     -- GenerateHtmlVariant.hs:238
{-# INLINE onafterprint #-}                                                     -- GenerateHtmlVariant.hs:239

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
onbeforeonload :: AttributeValue  -- ^ Attribute value.                         -- GenerateHtmlVariant.hs:236
               -> Attribute       -- ^ Resulting attribute.                     -- GenerateHtmlVariant.hs:237
onbeforeonload = attribute " onbeforeonload=\""                                 -- GenerateHtmlVariant.hs:238
{-# INLINE onbeforeonload #-}                                                   -- GenerateHtmlVariant.hs:239

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
onbeforeprint :: AttributeValue  -- ^ Attribute value.                          -- GenerateHtmlVariant.hs:236
              -> Attribute       -- ^ Resulting attribute.                      -- GenerateHtmlVariant.hs:237
onbeforeprint = attribute " onbeforeprint=\""                                   -- GenerateHtmlVariant.hs:238
{-# INLINE onbeforeprint #-}                                                    -- GenerateHtmlVariant.hs:239

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
onblur :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
onblur = attribute " onblur=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE onblur #-}                                                           -- GenerateHtmlVariant.hs:239

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
onerror :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onerror = attribute " onerror=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onerror #-}                                                          -- GenerateHtmlVariant.hs:239

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
onfocus :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onfocus = attribute " onfocus=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onfocus #-}                                                          -- GenerateHtmlVariant.hs:239

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
onhaschange :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
onhaschange = attribute " onhaschange=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE onhaschange #-}                                                      -- GenerateHtmlVariant.hs:239

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
onload :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
onload = attribute " onload=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE onload #-}                                                           -- GenerateHtmlVariant.hs:239

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
onmessage :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
onmessage = attribute " onmessage=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE onmessage #-}                                                        -- GenerateHtmlVariant.hs:239

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
onoffline :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
onoffline = attribute " onoffline=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE onoffline #-}                                                        -- GenerateHtmlVariant.hs:239

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
ononline :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
ononline = attribute " ononline=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE ononline #-}                                                         -- GenerateHtmlVariant.hs:239

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
onpagehide :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
onpagehide = attribute " onpagehide=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE onpagehide #-}                                                       -- GenerateHtmlVariant.hs:239

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
onpageshow :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
onpageshow = attribute " onpageshow=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE onpageshow #-}                                                       -- GenerateHtmlVariant.hs:239

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
onpropstate :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
onpropstate = attribute " onpropstate=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE onpropstate #-}                                                      -- GenerateHtmlVariant.hs:239

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
onredo :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
onredo = attribute " onredo=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE onredo #-}                                                           -- GenerateHtmlVariant.hs:239

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
onresize :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
onresize = attribute " onresize=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE onresize #-}                                                         -- GenerateHtmlVariant.hs:239

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
onstorage :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
onstorage = attribute " onstorage=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE onstorage #-}                                                        -- GenerateHtmlVariant.hs:239

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
onundo :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
onundo = attribute " onundo=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE onundo #-}                                                           -- GenerateHtmlVariant.hs:239

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
onunload :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
onunload = attribute " onunload=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE onunload #-}                                                         -- GenerateHtmlVariant.hs:239

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
open :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
open = attribute " open=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE open #-}                                                             -- GenerateHtmlVariant.hs:239

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
optimum :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
optimum = attribute " optimum=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE optimum #-}                                                          -- GenerateHtmlVariant.hs:239

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
pattern :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
pattern = attribute " pattern=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE pattern #-}                                                          -- GenerateHtmlVariant.hs:239

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
ping :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
ping = attribute " ping=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE ping #-}                                                             -- GenerateHtmlVariant.hs:239

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
placeholder :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
placeholder = attribute " placeholder=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE placeholder #-}                                                      -- GenerateHtmlVariant.hs:239

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
preload :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
preload = attribute " preload=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE preload #-}                                                          -- GenerateHtmlVariant.hs:239

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
pubdate :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
pubdate = attribute " pubdate=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE pubdate #-}                                                          -- GenerateHtmlVariant.hs:239

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
radiogroup :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
radiogroup = attribute " radiogroup=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE radiogroup #-}                                                       -- GenerateHtmlVariant.hs:239

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
readonly :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
readonly = attribute " readonly=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE readonly #-}                                                         -- GenerateHtmlVariant.hs:239

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
rel :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
rel = attribute " rel=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE rel #-}                                                              -- GenerateHtmlVariant.hs:239

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
required :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
required = attribute " required=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE required #-}                                                         -- GenerateHtmlVariant.hs:239

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
reversed :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
reversed = attribute " reversed=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE reversed #-}                                                         -- GenerateHtmlVariant.hs:239

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
rows :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
rows = attribute " rows=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE rows #-}                                                             -- GenerateHtmlVariant.hs:239

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
rowspan :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
rowspan = attribute " rowspan=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE rowspan #-}                                                          -- GenerateHtmlVariant.hs:239

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
sandbox :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
sandbox = attribute " sandbox=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE sandbox #-}                                                          -- GenerateHtmlVariant.hs:239

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
scope :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
scope = attribute " scope=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE scope #-}                                                            -- GenerateHtmlVariant.hs:239

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
scoped :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
scoped = attribute " scoped=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE scoped #-}                                                           -- GenerateHtmlVariant.hs:239

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
seamless :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
seamless = attribute " seamless=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE seamless #-}                                                         -- GenerateHtmlVariant.hs:239

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
selected :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
selected = attribute " selected=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE selected #-}                                                         -- GenerateHtmlVariant.hs:239

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
shape :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
shape = attribute " shape=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE shape #-}                                                            -- GenerateHtmlVariant.hs:239

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
size :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
size = attribute " size=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE size #-}                                                             -- GenerateHtmlVariant.hs:239

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
sizes :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
sizes = attribute " sizes=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE sizes #-}                                                            -- GenerateHtmlVariant.hs:239

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
span :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
span = attribute " span=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE span #-}                                                             -- GenerateHtmlVariant.hs:239

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
spellcheck :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
spellcheck = attribute " spellcheck=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE spellcheck #-}                                                       -- GenerateHtmlVariant.hs:239

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
src :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
src = attribute " src=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE src #-}                                                              -- GenerateHtmlVariant.hs:239

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
srcdoc :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
srcdoc = attribute " srcdoc=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE srcdoc #-}                                                           -- GenerateHtmlVariant.hs:239

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
start :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
start = attribute " start=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE start #-}                                                            -- GenerateHtmlVariant.hs:239

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
step :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
step = attribute " step=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE step #-}                                                             -- GenerateHtmlVariant.hs:239

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
style :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
style = attribute " style=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE style #-}                                                            -- GenerateHtmlVariant.hs:239

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
subject :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
subject = attribute " subject=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE subject #-}                                                          -- GenerateHtmlVariant.hs:239

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
summary :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
summary = attribute " summary=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE summary #-}                                                          -- GenerateHtmlVariant.hs:239

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
tabindex :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
tabindex = attribute " tabindex=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE tabindex #-}                                                         -- GenerateHtmlVariant.hs:239

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
target :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
target = attribute " target=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE target #-}                                                           -- GenerateHtmlVariant.hs:239

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
title :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
title = attribute " title=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE title #-}                                                            -- GenerateHtmlVariant.hs:239

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
type_ :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
type_ = attribute " type=\""                                                    -- GenerateHtmlVariant.hs:238
{-# INLINE type_ #-}                                                            -- GenerateHtmlVariant.hs:239

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
usemap :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
usemap = attribute " usemap=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE usemap #-}                                                           -- GenerateHtmlVariant.hs:239

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
value :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
value = attribute " value=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE value #-}                                                            -- GenerateHtmlVariant.hs:239

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
width :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
width = attribute " width=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE width #-}                                                            -- GenerateHtmlVariant.hs:239

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
wrap :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
wrap = attribute " wrap=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE wrap #-}                                                             -- GenerateHtmlVariant.hs:239

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
xmlns :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
xmlns = attribute " xmlns=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE xmlns #-}                                                            -- GenerateHtmlVariant.hs:239
