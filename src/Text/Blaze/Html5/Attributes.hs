-- WARNING: This code was automatically generated. You should *never*
-- edit it directly. Instead, edit the files who generated this code,
-- you can find them in the @util/@ directory.

{-# LANGUAGE OverloadedStrings #-}                                              -- util/GenerateHtmlVariant.hs:71
-- | This module exports combinators that provide you with the
-- ability to set attributes on HTML elements.
--
module Text.Blaze.Html5.Attributes                                              -- util/GenerateHtmlVariant.hs:120
    ( accept                                                                    -- util/GenerateHtmlVariant.hs:121
    , accept_charset                                                            -- util/GenerateHtmlVariant.hs:123
    , accesskey                                                                 -- util/GenerateHtmlVariant.hs:123
    , action                                                                    -- util/GenerateHtmlVariant.hs:123
    , alt                                                                       -- util/GenerateHtmlVariant.hs:123
    , async                                                                     -- util/GenerateHtmlVariant.hs:123
    , autocomplete                                                              -- util/GenerateHtmlVariant.hs:123
    , autofocus                                                                 -- util/GenerateHtmlVariant.hs:123
    , autoplay                                                                  -- util/GenerateHtmlVariant.hs:123
    , challenge                                                                 -- util/GenerateHtmlVariant.hs:123
    , charset                                                                   -- util/GenerateHtmlVariant.hs:123
    , checked                                                                   -- util/GenerateHtmlVariant.hs:123
    , cite                                                                      -- util/GenerateHtmlVariant.hs:123
    , class_                                                                    -- util/GenerateHtmlVariant.hs:123
    , cols                                                                      -- util/GenerateHtmlVariant.hs:123
    , colspan                                                                   -- util/GenerateHtmlVariant.hs:123
    , content                                                                   -- util/GenerateHtmlVariant.hs:123
    , contenteditable                                                           -- util/GenerateHtmlVariant.hs:123
    , contextmenu                                                               -- util/GenerateHtmlVariant.hs:123
    , controls                                                                  -- util/GenerateHtmlVariant.hs:123
    , coords                                                                    -- util/GenerateHtmlVariant.hs:123
    , data_                                                                     -- util/GenerateHtmlVariant.hs:123
    , datetime                                                                  -- util/GenerateHtmlVariant.hs:123
    , defer                                                                     -- util/GenerateHtmlVariant.hs:123
    , dir                                                                       -- util/GenerateHtmlVariant.hs:123
    , disabled                                                                  -- util/GenerateHtmlVariant.hs:123
    , draggable                                                                 -- util/GenerateHtmlVariant.hs:123
    , enctype                                                                   -- util/GenerateHtmlVariant.hs:123
    , for                                                                       -- util/GenerateHtmlVariant.hs:123
    , form                                                                      -- util/GenerateHtmlVariant.hs:123
    , formaction                                                                -- util/GenerateHtmlVariant.hs:123
    , formenctype                                                               -- util/GenerateHtmlVariant.hs:123
    , formmethod                                                                -- util/GenerateHtmlVariant.hs:123
    , formnovalidate                                                            -- util/GenerateHtmlVariant.hs:123
    , formtarget                                                                -- util/GenerateHtmlVariant.hs:123
    , headers                                                                   -- util/GenerateHtmlVariant.hs:123
    , height                                                                    -- util/GenerateHtmlVariant.hs:123
    , hidden                                                                    -- util/GenerateHtmlVariant.hs:123
    , high                                                                      -- util/GenerateHtmlVariant.hs:123
    , href                                                                      -- util/GenerateHtmlVariant.hs:123
    , hreflang                                                                  -- util/GenerateHtmlVariant.hs:123
    , http_equiv                                                                -- util/GenerateHtmlVariant.hs:123
    , icon                                                                      -- util/GenerateHtmlVariant.hs:123
    , id                                                                        -- util/GenerateHtmlVariant.hs:123
    , ismap                                                                     -- util/GenerateHtmlVariant.hs:123
    , item                                                                      -- util/GenerateHtmlVariant.hs:123
    , itemprop                                                                  -- util/GenerateHtmlVariant.hs:123
    , keytype                                                                   -- util/GenerateHtmlVariant.hs:123
    , label                                                                     -- util/GenerateHtmlVariant.hs:123
    , lang                                                                      -- util/GenerateHtmlVariant.hs:123
    , list                                                                      -- util/GenerateHtmlVariant.hs:123
    , loop                                                                      -- util/GenerateHtmlVariant.hs:123
    , low                                                                       -- util/GenerateHtmlVariant.hs:123
    , manifest                                                                  -- util/GenerateHtmlVariant.hs:123
    , max                                                                       -- util/GenerateHtmlVariant.hs:123
    , maxlength                                                                 -- util/GenerateHtmlVariant.hs:123
    , media                                                                     -- util/GenerateHtmlVariant.hs:123
    , method                                                                    -- util/GenerateHtmlVariant.hs:123
    , min                                                                       -- util/GenerateHtmlVariant.hs:123
    , multiple                                                                  -- util/GenerateHtmlVariant.hs:123
    , name                                                                      -- util/GenerateHtmlVariant.hs:123
    , novalidate                                                                -- util/GenerateHtmlVariant.hs:123
    , onafterprint                                                              -- util/GenerateHtmlVariant.hs:123
    , onbeforeonload                                                            -- util/GenerateHtmlVariant.hs:123
    , onbeforeprint                                                             -- util/GenerateHtmlVariant.hs:123
    , onblur                                                                    -- util/GenerateHtmlVariant.hs:123
    , onerror                                                                   -- util/GenerateHtmlVariant.hs:123
    , onfocus                                                                   -- util/GenerateHtmlVariant.hs:123
    , onhaschange                                                               -- util/GenerateHtmlVariant.hs:123
    , onload                                                                    -- util/GenerateHtmlVariant.hs:123
    , onmessage                                                                 -- util/GenerateHtmlVariant.hs:123
    , onoffline                                                                 -- util/GenerateHtmlVariant.hs:123
    , ononline                                                                  -- util/GenerateHtmlVariant.hs:123
    , onpagehide                                                                -- util/GenerateHtmlVariant.hs:123
    , onpageshow                                                                -- util/GenerateHtmlVariant.hs:123
    , onpropstate                                                               -- util/GenerateHtmlVariant.hs:123
    , onredo                                                                    -- util/GenerateHtmlVariant.hs:123
    , onresize                                                                  -- util/GenerateHtmlVariant.hs:123
    , onstorage                                                                 -- util/GenerateHtmlVariant.hs:123
    , onundo                                                                    -- util/GenerateHtmlVariant.hs:123
    , onunload                                                                  -- util/GenerateHtmlVariant.hs:123
    , open                                                                      -- util/GenerateHtmlVariant.hs:123
    , optimum                                                                   -- util/GenerateHtmlVariant.hs:123
    , pattern                                                                   -- util/GenerateHtmlVariant.hs:123
    , ping                                                                      -- util/GenerateHtmlVariant.hs:123
    , placeholder                                                               -- util/GenerateHtmlVariant.hs:123
    , preload                                                                   -- util/GenerateHtmlVariant.hs:123
    , pubdate                                                                   -- util/GenerateHtmlVariant.hs:123
    , radiogroup                                                                -- util/GenerateHtmlVariant.hs:123
    , readonly                                                                  -- util/GenerateHtmlVariant.hs:123
    , rel                                                                       -- util/GenerateHtmlVariant.hs:123
    , required                                                                  -- util/GenerateHtmlVariant.hs:123
    , reversed                                                                  -- util/GenerateHtmlVariant.hs:123
    , rows                                                                      -- util/GenerateHtmlVariant.hs:123
    , rowspan                                                                   -- util/GenerateHtmlVariant.hs:123
    , sandbox                                                                   -- util/GenerateHtmlVariant.hs:123
    , scope                                                                     -- util/GenerateHtmlVariant.hs:123
    , scoped                                                                    -- util/GenerateHtmlVariant.hs:123
    , seamless                                                                  -- util/GenerateHtmlVariant.hs:123
    , selected                                                                  -- util/GenerateHtmlVariant.hs:123
    , shape                                                                     -- util/GenerateHtmlVariant.hs:123
    , size                                                                      -- util/GenerateHtmlVariant.hs:123
    , sizes                                                                     -- util/GenerateHtmlVariant.hs:123
    , span                                                                      -- util/GenerateHtmlVariant.hs:123
    , spellcheck                                                                -- util/GenerateHtmlVariant.hs:123
    , src                                                                       -- util/GenerateHtmlVariant.hs:123
    , srcdoc                                                                    -- util/GenerateHtmlVariant.hs:123
    , start                                                                     -- util/GenerateHtmlVariant.hs:123
    , step                                                                      -- util/GenerateHtmlVariant.hs:123
    , style                                                                     -- util/GenerateHtmlVariant.hs:123
    , subject                                                                   -- util/GenerateHtmlVariant.hs:123
    , summary                                                                   -- util/GenerateHtmlVariant.hs:123
    , tabindex                                                                  -- util/GenerateHtmlVariant.hs:123
    , target                                                                    -- util/GenerateHtmlVariant.hs:123
    , title                                                                     -- util/GenerateHtmlVariant.hs:123
    , type_                                                                     -- util/GenerateHtmlVariant.hs:123
    , usemap                                                                    -- util/GenerateHtmlVariant.hs:123
    , value                                                                     -- util/GenerateHtmlVariant.hs:123
    , width                                                                     -- util/GenerateHtmlVariant.hs:123
    , wrap                                                                      -- util/GenerateHtmlVariant.hs:123
    , xmlns                                                                     -- util/GenerateHtmlVariant.hs:123
    ) where                                                                     -- util/GenerateHtmlVariant.hs:124

import Prelude ()                                                               -- util/GenerateHtmlVariant.hs:76
                                                                                -- util/GenerateHtmlVariant.hs:77
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)               -- util/GenerateHtmlVariant.hs:78
                                                                                -- util/GenerateHtmlVariant.hs:79
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
accept :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
accept = attribute " accept=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE accept #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
accept_charset :: AttributeValue  -- ^ Attribute value.                         -- util/GenerateHtmlVariant.hs:245
               -> Attribute       -- ^ Resulting attribute.                     -- util/GenerateHtmlVariant.hs:246
accept_charset = attribute " accept-charset=\""                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE accept_charset #-}                                                   -- util/GenerateHtmlVariant.hs:248

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
accesskey :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
accesskey = attribute " accesskey=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE accesskey #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
action :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
action = attribute " action=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE action #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
alt :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
alt = attribute " alt=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE alt #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
async :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
async = attribute " async=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE async #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
autocomplete :: AttributeValue  -- ^ Attribute value.                           -- util/GenerateHtmlVariant.hs:245
             -> Attribute       -- ^ Resulting attribute.                       -- util/GenerateHtmlVariant.hs:246
autocomplete = attribute " autocomplete=\""                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE autocomplete #-}                                                     -- util/GenerateHtmlVariant.hs:248

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
autofocus :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
autofocus = attribute " autofocus=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE autofocus #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
autoplay :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
autoplay = attribute " autoplay=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE autoplay #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
challenge :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
challenge = attribute " challenge=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE challenge #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
charset :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
charset = attribute " charset=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE charset #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
checked :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
checked = attribute " checked=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE checked #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
cite :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
cite = attribute " cite=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE cite #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
class_ :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
class_ = attribute " class=\""                                                  -- util/GenerateHtmlVariant.hs:247
{-# INLINE class_ #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
cols :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
cols = attribute " cols=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE cols #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
colspan :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
colspan = attribute " colspan=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE colspan #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
content :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
content = attribute " content=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE content #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
contenteditable :: AttributeValue  -- ^ Attribute value.                        -- util/GenerateHtmlVariant.hs:245
                -> Attribute       -- ^ Resulting attribute.                    -- util/GenerateHtmlVariant.hs:246
contenteditable = attribute " contenteditable=\""                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE contenteditable #-}                                                  -- util/GenerateHtmlVariant.hs:248

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
contextmenu :: AttributeValue  -- ^ Attribute value.                            -- util/GenerateHtmlVariant.hs:245
            -> Attribute       -- ^ Resulting attribute.                        -- util/GenerateHtmlVariant.hs:246
contextmenu = attribute " contextmenu=\""                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE contextmenu #-}                                                      -- util/GenerateHtmlVariant.hs:248

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
controls :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
controls = attribute " controls=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE controls #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
coords :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
coords = attribute " coords=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE coords #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
data_ :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
data_ = attribute " data=\""                                                    -- util/GenerateHtmlVariant.hs:247
{-# INLINE data_ #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
datetime :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
datetime = attribute " datetime=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE datetime #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
defer :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
defer = attribute " defer=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE defer #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
dir :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
dir = attribute " dir=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE dir #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
disabled :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
disabled = attribute " disabled=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE disabled #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
draggable :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
draggable = attribute " draggable=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE draggable #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
enctype :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
enctype = attribute " enctype=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE enctype #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
for :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
for = attribute " for=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE for #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
form :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
form = attribute " form=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE form #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
formaction :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
formaction = attribute " formaction=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE formaction #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
formenctype :: AttributeValue  -- ^ Attribute value.                            -- util/GenerateHtmlVariant.hs:245
            -> Attribute       -- ^ Resulting attribute.                        -- util/GenerateHtmlVariant.hs:246
formenctype = attribute " formenctype=\""                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE formenctype #-}                                                      -- util/GenerateHtmlVariant.hs:248

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
formmethod :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
formmethod = attribute " formmethod=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE formmethod #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
formnovalidate :: AttributeValue  -- ^ Attribute value.                         -- util/GenerateHtmlVariant.hs:245
               -> Attribute       -- ^ Resulting attribute.                     -- util/GenerateHtmlVariant.hs:246
formnovalidate = attribute " formnovalidate=\""                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE formnovalidate #-}                                                   -- util/GenerateHtmlVariant.hs:248

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
formtarget :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
formtarget = attribute " formtarget=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE formtarget #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
headers :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
headers = attribute " headers=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE headers #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
height :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
height = attribute " height=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE height #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
hidden :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
hidden = attribute " hidden=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE hidden #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
high :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
high = attribute " high=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE high #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
href :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
href = attribute " href=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE href #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
hreflang :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
hreflang = attribute " hreflang=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE hreflang #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
http_equiv :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
http_equiv = attribute " http-equiv=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE http_equiv #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
icon :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
icon = attribute " icon=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE icon #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
id :: AttributeValue  -- ^ Attribute value.                                     -- util/GenerateHtmlVariant.hs:245
   -> Attribute       -- ^ Resulting attribute.                                 -- util/GenerateHtmlVariant.hs:246
id = attribute " id=\""                                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE id #-}                                                               -- util/GenerateHtmlVariant.hs:248

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
ismap :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
ismap = attribute " ismap=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE ismap #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
item :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
item = attribute " item=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE item #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
itemprop :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
itemprop = attribute " itemprop=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE itemprop #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
keytype :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
keytype = attribute " keytype=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE keytype #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
label :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
label = attribute " label=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE label #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
lang :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
lang = attribute " lang=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE lang #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
list :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
list = attribute " list=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE list #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
loop :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
loop = attribute " loop=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE loop #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
low :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
low = attribute " low=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE low #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
manifest :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
manifest = attribute " manifest=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE manifest #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
max :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
max = attribute " max=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE max #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
maxlength :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
maxlength = attribute " maxlength=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE maxlength #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
media :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
media = attribute " media=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE media #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
method :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
method = attribute " method=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE method #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
min :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
min = attribute " min=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE min #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
multiple :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
multiple = attribute " multiple=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE multiple #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
name :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
name = attribute " name=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE name #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
novalidate :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
novalidate = attribute " novalidate=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE novalidate #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
onafterprint :: AttributeValue  -- ^ Attribute value.                           -- util/GenerateHtmlVariant.hs:245
             -> Attribute       -- ^ Resulting attribute.                       -- util/GenerateHtmlVariant.hs:246
onafterprint = attribute " onafterprint=\""                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE onafterprint #-}                                                     -- util/GenerateHtmlVariant.hs:248

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
onbeforeonload :: AttributeValue  -- ^ Attribute value.                         -- util/GenerateHtmlVariant.hs:245
               -> Attribute       -- ^ Resulting attribute.                     -- util/GenerateHtmlVariant.hs:246
onbeforeonload = attribute " onbeforeonload=\""                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE onbeforeonload #-}                                                   -- util/GenerateHtmlVariant.hs:248

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
onbeforeprint :: AttributeValue  -- ^ Attribute value.                          -- util/GenerateHtmlVariant.hs:245
              -> Attribute       -- ^ Resulting attribute.                      -- util/GenerateHtmlVariant.hs:246
onbeforeprint = attribute " onbeforeprint=\""                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE onbeforeprint #-}                                                    -- util/GenerateHtmlVariant.hs:248

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
onblur :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
onblur = attribute " onblur=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE onblur #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
onerror :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
onerror = attribute " onerror=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE onerror #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
onfocus :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
onfocus = attribute " onfocus=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE onfocus #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
onhaschange :: AttributeValue  -- ^ Attribute value.                            -- util/GenerateHtmlVariant.hs:245
            -> Attribute       -- ^ Resulting attribute.                        -- util/GenerateHtmlVariant.hs:246
onhaschange = attribute " onhaschange=\""                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE onhaschange #-}                                                      -- util/GenerateHtmlVariant.hs:248

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
onload :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
onload = attribute " onload=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE onload #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
onmessage :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
onmessage = attribute " onmessage=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE onmessage #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
onoffline :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
onoffline = attribute " onoffline=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE onoffline #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
ononline :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
ononline = attribute " ononline=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE ononline #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
onpagehide :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
onpagehide = attribute " onpagehide=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE onpagehide #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
onpageshow :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
onpageshow = attribute " onpageshow=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE onpageshow #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
onpropstate :: AttributeValue  -- ^ Attribute value.                            -- util/GenerateHtmlVariant.hs:245
            -> Attribute       -- ^ Resulting attribute.                        -- util/GenerateHtmlVariant.hs:246
onpropstate = attribute " onpropstate=\""                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE onpropstate #-}                                                      -- util/GenerateHtmlVariant.hs:248

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
onredo :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
onredo = attribute " onredo=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE onredo #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
onresize :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
onresize = attribute " onresize=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE onresize #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
onstorage :: AttributeValue  -- ^ Attribute value.                              -- util/GenerateHtmlVariant.hs:245
          -> Attribute       -- ^ Resulting attribute.                          -- util/GenerateHtmlVariant.hs:246
onstorage = attribute " onstorage=\""                                           -- util/GenerateHtmlVariant.hs:247
{-# INLINE onstorage #-}                                                        -- util/GenerateHtmlVariant.hs:248

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
onundo :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
onundo = attribute " onundo=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE onundo #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
onunload :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
onunload = attribute " onunload=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE onunload #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
open :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
open = attribute " open=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE open #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
optimum :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
optimum = attribute " optimum=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE optimum #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
pattern :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
pattern = attribute " pattern=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE pattern #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
ping :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
ping = attribute " ping=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE ping #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
placeholder :: AttributeValue  -- ^ Attribute value.                            -- util/GenerateHtmlVariant.hs:245
            -> Attribute       -- ^ Resulting attribute.                        -- util/GenerateHtmlVariant.hs:246
placeholder = attribute " placeholder=\""                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE placeholder #-}                                                      -- util/GenerateHtmlVariant.hs:248

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
preload :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
preload = attribute " preload=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE preload #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
pubdate :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
pubdate = attribute " pubdate=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE pubdate #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
radiogroup :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
radiogroup = attribute " radiogroup=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE radiogroup #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
readonly :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
readonly = attribute " readonly=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE readonly #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
rel :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
rel = attribute " rel=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE rel #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
required :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
required = attribute " required=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE required #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
reversed :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
reversed = attribute " reversed=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE reversed #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
rows :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
rows = attribute " rows=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE rows #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
rowspan :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
rowspan = attribute " rowspan=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE rowspan #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
sandbox :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
sandbox = attribute " sandbox=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE sandbox #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
scope :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
scope = attribute " scope=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE scope #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
scoped :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
scoped = attribute " scoped=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE scoped #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
seamless :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
seamless = attribute " seamless=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE seamless #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
selected :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
selected = attribute " selected=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE selected #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
shape :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
shape = attribute " shape=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE shape #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
size :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
size = attribute " size=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE size #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
sizes :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
sizes = attribute " sizes=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE sizes #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
span :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
span = attribute " span=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE span #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
spellcheck :: AttributeValue  -- ^ Attribute value.                             -- util/GenerateHtmlVariant.hs:245
           -> Attribute       -- ^ Resulting attribute.                         -- util/GenerateHtmlVariant.hs:246
spellcheck = attribute " spellcheck=\""                                         -- util/GenerateHtmlVariant.hs:247
{-# INLINE spellcheck #-}                                                       -- util/GenerateHtmlVariant.hs:248

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
src :: AttributeValue  -- ^ Attribute value.                                    -- util/GenerateHtmlVariant.hs:245
    -> Attribute       -- ^ Resulting attribute.                                -- util/GenerateHtmlVariant.hs:246
src = attribute " src=\""                                                       -- util/GenerateHtmlVariant.hs:247
{-# INLINE src #-}                                                              -- util/GenerateHtmlVariant.hs:248

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
srcdoc :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
srcdoc = attribute " srcdoc=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE srcdoc #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
start :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
start = attribute " start=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE start #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
step :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
step = attribute " step=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE step #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
style :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
style = attribute " style=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE style #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
subject :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
subject = attribute " subject=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE subject #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
summary :: AttributeValue  -- ^ Attribute value.                                -- util/GenerateHtmlVariant.hs:245
        -> Attribute       -- ^ Resulting attribute.                            -- util/GenerateHtmlVariant.hs:246
summary = attribute " summary=\""                                               -- util/GenerateHtmlVariant.hs:247
{-# INLINE summary #-}                                                          -- util/GenerateHtmlVariant.hs:248

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
tabindex :: AttributeValue  -- ^ Attribute value.                               -- util/GenerateHtmlVariant.hs:245
         -> Attribute       -- ^ Resulting attribute.                           -- util/GenerateHtmlVariant.hs:246
tabindex = attribute " tabindex=\""                                             -- util/GenerateHtmlVariant.hs:247
{-# INLINE tabindex #-}                                                         -- util/GenerateHtmlVariant.hs:248

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
target :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
target = attribute " target=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE target #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
title :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
title = attribute " title=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE title #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
type_ :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
type_ = attribute " type=\""                                                    -- util/GenerateHtmlVariant.hs:247
{-# INLINE type_ #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
usemap :: AttributeValue  -- ^ Attribute value.                                 -- util/GenerateHtmlVariant.hs:245
       -> Attribute       -- ^ Resulting attribute.                             -- util/GenerateHtmlVariant.hs:246
usemap = attribute " usemap=\""                                                 -- util/GenerateHtmlVariant.hs:247
{-# INLINE usemap #-}                                                           -- util/GenerateHtmlVariant.hs:248

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
value :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
value = attribute " value=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE value #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
width :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
width = attribute " width=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE width #-}                                                            -- util/GenerateHtmlVariant.hs:248

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
wrap :: AttributeValue  -- ^ Attribute value.                                   -- util/GenerateHtmlVariant.hs:245
     -> Attribute       -- ^ Resulting attribute.                               -- util/GenerateHtmlVariant.hs:246
wrap = attribute " wrap=\""                                                     -- util/GenerateHtmlVariant.hs:247
{-# INLINE wrap #-}                                                             -- util/GenerateHtmlVariant.hs:248

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
xmlns :: AttributeValue  -- ^ Attribute value.                                  -- util/GenerateHtmlVariant.hs:245
      -> Attribute       -- ^ Resulting attribute.                              -- util/GenerateHtmlVariant.hs:246
xmlns = attribute " xmlns=\""                                                   -- util/GenerateHtmlVariant.hs:247
{-# INLINE xmlns #-}                                                            -- util/GenerateHtmlVariant.hs:248
