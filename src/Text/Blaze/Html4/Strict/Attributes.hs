{-# LANGUAGE OverloadedStrings #-}                                              -- GenerateHtmlVariant.hs:69
-- | This module exports combinators that provide you with the
-- ability to set attributes on HTML elements.
--
module Text.Blaze.Html4.Strict.Attributes                                       -- GenerateHtmlVariant.hs:111
    ( abbr                                                                      -- GenerateHtmlVariant.hs:112
    , accept                                                                    -- GenerateHtmlVariant.hs:114
    , accesskey                                                                 -- GenerateHtmlVariant.hs:114
    , action                                                                    -- GenerateHtmlVariant.hs:114
    , align                                                                     -- GenerateHtmlVariant.hs:114
    , alt                                                                       -- GenerateHtmlVariant.hs:114
    , archive                                                                   -- GenerateHtmlVariant.hs:114
    , axis                                                                      -- GenerateHtmlVariant.hs:114
    , border                                                                    -- GenerateHtmlVariant.hs:114
    , cellpadding                                                               -- GenerateHtmlVariant.hs:114
    , cellspacing                                                               -- GenerateHtmlVariant.hs:114
    , char                                                                      -- GenerateHtmlVariant.hs:114
    , charoff                                                                   -- GenerateHtmlVariant.hs:114
    , charset                                                                   -- GenerateHtmlVariant.hs:114
    , checked                                                                   -- GenerateHtmlVariant.hs:114
    , cite                                                                      -- GenerateHtmlVariant.hs:114
    , class_                                                                    -- GenerateHtmlVariant.hs:114
    , classid                                                                   -- GenerateHtmlVariant.hs:114
    , codebase                                                                  -- GenerateHtmlVariant.hs:114
    , codetype                                                                  -- GenerateHtmlVariant.hs:114
    , cols                                                                      -- GenerateHtmlVariant.hs:114
    , colspan                                                                   -- GenerateHtmlVariant.hs:114
    , content                                                                   -- GenerateHtmlVariant.hs:114
    , coords                                                                    -- GenerateHtmlVariant.hs:114
    , data_                                                                     -- GenerateHtmlVariant.hs:114
    , datetime                                                                  -- GenerateHtmlVariant.hs:114
    , declare                                                                   -- GenerateHtmlVariant.hs:114
    , defer                                                                     -- GenerateHtmlVariant.hs:114
    , dir                                                                       -- GenerateHtmlVariant.hs:114
    , disabled                                                                  -- GenerateHtmlVariant.hs:114
    , for                                                                       -- GenerateHtmlVariant.hs:114
    , frame                                                                     -- GenerateHtmlVariant.hs:114
    , headers                                                                   -- GenerateHtmlVariant.hs:114
    , height                                                                    -- GenerateHtmlVariant.hs:114
    , href                                                                      -- GenerateHtmlVariant.hs:114
    , hreflang                                                                  -- GenerateHtmlVariant.hs:114
    , http_equiv                                                                -- GenerateHtmlVariant.hs:114
    , id                                                                        -- GenerateHtmlVariant.hs:114
    , label                                                                     -- GenerateHtmlVariant.hs:114
    , lang                                                                      -- GenerateHtmlVariant.hs:114
    , maxlength                                                                 -- GenerateHtmlVariant.hs:114
    , media                                                                     -- GenerateHtmlVariant.hs:114
    , method                                                                    -- GenerateHtmlVariant.hs:114
    , multiple                                                                  -- GenerateHtmlVariant.hs:114
    , name                                                                      -- GenerateHtmlVariant.hs:114
    , nohref                                                                    -- GenerateHtmlVariant.hs:114
    , onabort                                                                   -- GenerateHtmlVariant.hs:114
    , onblur                                                                    -- GenerateHtmlVariant.hs:114
    , onchange                                                                  -- GenerateHtmlVariant.hs:114
    , onclick                                                                   -- GenerateHtmlVariant.hs:114
    , ondblclick                                                                -- GenerateHtmlVariant.hs:114
    , onfocus                                                                   -- GenerateHtmlVariant.hs:114
    , onkeydown                                                                 -- GenerateHtmlVariant.hs:114
    , onkeypress                                                                -- GenerateHtmlVariant.hs:114
    , onkeyup                                                                   -- GenerateHtmlVariant.hs:114
    , onload                                                                    -- GenerateHtmlVariant.hs:114
    , onmousedown                                                               -- GenerateHtmlVariant.hs:114
    , onmousemove                                                               -- GenerateHtmlVariant.hs:114
    , onmouseout                                                                -- GenerateHtmlVariant.hs:114
    , onmouseover                                                               -- GenerateHtmlVariant.hs:114
    , onmouseup                                                                 -- GenerateHtmlVariant.hs:114
    , onreset                                                                   -- GenerateHtmlVariant.hs:114
    , onselect                                                                  -- GenerateHtmlVariant.hs:114
    , onsubmit                                                                  -- GenerateHtmlVariant.hs:114
    , onunload                                                                  -- GenerateHtmlVariant.hs:114
    , profile                                                                   -- GenerateHtmlVariant.hs:114
    , readonly                                                                  -- GenerateHtmlVariant.hs:114
    , rel                                                                       -- GenerateHtmlVariant.hs:114
    , rev                                                                       -- GenerateHtmlVariant.hs:114
    , rows                                                                      -- GenerateHtmlVariant.hs:114
    , rowspan                                                                   -- GenerateHtmlVariant.hs:114
    , rules                                                                     -- GenerateHtmlVariant.hs:114
    , scheme                                                                    -- GenerateHtmlVariant.hs:114
    , scope                                                                     -- GenerateHtmlVariant.hs:114
    , selected                                                                  -- GenerateHtmlVariant.hs:114
    , shape                                                                     -- GenerateHtmlVariant.hs:114
    , size                                                                      -- GenerateHtmlVariant.hs:114
    , span                                                                      -- GenerateHtmlVariant.hs:114
    , src                                                                       -- GenerateHtmlVariant.hs:114
    , standby                                                                   -- GenerateHtmlVariant.hs:114
    , style                                                                     -- GenerateHtmlVariant.hs:114
    , summary                                                                   -- GenerateHtmlVariant.hs:114
    , tabindex                                                                  -- GenerateHtmlVariant.hs:114
    , title                                                                     -- GenerateHtmlVariant.hs:114
    , type_                                                                     -- GenerateHtmlVariant.hs:114
    , usemap                                                                    -- GenerateHtmlVariant.hs:114
    , valign                                                                    -- GenerateHtmlVariant.hs:114
    , value                                                                     -- GenerateHtmlVariant.hs:114
    , valuetype                                                                 -- GenerateHtmlVariant.hs:114
    , width                                                                     -- GenerateHtmlVariant.hs:114
    ) where                                                                     -- GenerateHtmlVariant.hs:115

import Prelude ()                                                               -- GenerateHtmlVariant.hs:74
                                                                                -- GenerateHtmlVariant.hs:75
import Data.Text (Text)                                                         -- GenerateHtmlVariant.hs:76
                                                                                -- GenerateHtmlVariant.hs:77
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)               -- GenerateHtmlVariant.hs:78
                                                                                -- GenerateHtmlVariant.hs:79
-- | Combinator for the @abbr@ attribute.
--
-- Example:
--
-- > div ! abbr "bar" $ "Hello."
--
-- Result:
--
-- > <div abbr="bar">Hello.</div>
--
abbr :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
abbr = attribute " abbr=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE abbr #-}                                                             -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @align@ attribute.
--
-- Example:
--
-- > div ! align "bar" $ "Hello."
--
-- Result:
--
-- > <div align="bar">Hello.</div>
--
align :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
align = attribute " align=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE align #-}                                                            -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @archive@ attribute.
--
-- Example:
--
-- > div ! archive "bar" $ "Hello."
--
-- Result:
--
-- > <div archive="bar">Hello.</div>
--
archive :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
archive = attribute " archive=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE archive #-}                                                          -- GenerateHtmlVariant.hs:239

-- | Combinator for the @axis@ attribute.
--
-- Example:
--
-- > div ! axis "bar" $ "Hello."
--
-- Result:
--
-- > <div axis="bar">Hello.</div>
--
axis :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
axis = attribute " axis=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE axis #-}                                                             -- GenerateHtmlVariant.hs:239

-- | Combinator for the @border@ attribute.
--
-- Example:
--
-- > div ! border "bar" $ "Hello."
--
-- Result:
--
-- > <div border="bar">Hello.</div>
--
border :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
border = attribute " border=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE border #-}                                                           -- GenerateHtmlVariant.hs:239

-- | Combinator for the @cellpadding@ attribute.
--
-- Example:
--
-- > div ! cellpadding "bar" $ "Hello."
--
-- Result:
--
-- > <div cellpadding="bar">Hello.</div>
--
cellpadding :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
cellpadding = attribute " cellpadding=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE cellpadding #-}                                                      -- GenerateHtmlVariant.hs:239

-- | Combinator for the @cellspacing@ attribute.
--
-- Example:
--
-- > div ! cellspacing "bar" $ "Hello."
--
-- Result:
--
-- > <div cellspacing="bar">Hello.</div>
--
cellspacing :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
cellspacing = attribute " cellspacing=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE cellspacing #-}                                                      -- GenerateHtmlVariant.hs:239

-- | Combinator for the @char@ attribute.
--
-- Example:
--
-- > div ! char "bar" $ "Hello."
--
-- Result:
--
-- > <div char="bar">Hello.</div>
--
char :: AttributeValue  -- ^ Attribute value.                                   -- GenerateHtmlVariant.hs:236
     -> Attribute       -- ^ Resulting attribute.                               -- GenerateHtmlVariant.hs:237
char = attribute " char=\""                                                     -- GenerateHtmlVariant.hs:238
{-# INLINE char #-}                                                             -- GenerateHtmlVariant.hs:239

-- | Combinator for the @charoff@ attribute.
--
-- Example:
--
-- > div ! charoff "bar" $ "Hello."
--
-- Result:
--
-- > <div charoff="bar">Hello.</div>
--
charoff :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
charoff = attribute " charoff=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE charoff #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @classid@ attribute.
--
-- Example:
--
-- > div ! classid "bar" $ "Hello."
--
-- Result:
--
-- > <div classid="bar">Hello.</div>
--
classid :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
classid = attribute " classid=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE classid #-}                                                          -- GenerateHtmlVariant.hs:239

-- | Combinator for the @codebase@ attribute.
--
-- Example:
--
-- > div ! codebase "bar" $ "Hello."
--
-- Result:
--
-- > <div codebase="bar">Hello.</div>
--
codebase :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
codebase = attribute " codebase=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE codebase #-}                                                         -- GenerateHtmlVariant.hs:239

-- | Combinator for the @codetype@ attribute.
--
-- Example:
--
-- > div ! codetype "bar" $ "Hello."
--
-- Result:
--
-- > <div codetype="bar">Hello.</div>
--
codetype :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
codetype = attribute " codetype=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE codetype #-}                                                         -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @declare@ attribute.
--
-- Example:
--
-- > div ! declare "bar" $ "Hello."
--
-- Result:
--
-- > <div declare="bar">Hello.</div>
--
declare :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
declare = attribute " declare=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE declare #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @frame@ attribute.
--
-- Example:
--
-- > div ! frame "bar" $ "Hello."
--
-- Result:
--
-- > <div frame="bar">Hello.</div>
--
frame :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
frame = attribute " frame=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE frame #-}                                                            -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @nohref@ attribute.
--
-- Example:
--
-- > div ! nohref "bar" $ "Hello."
--
-- Result:
--
-- > <div nohref="bar">Hello.</div>
--
nohref :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
nohref = attribute " nohref=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE nohref #-}                                                           -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onabort@ attribute.
--
-- Example:
--
-- > div ! onabort "bar" $ "Hello."
--
-- Result:
--
-- > <div onabort="bar">Hello.</div>
--
onabort :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onabort = attribute " onabort=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onabort #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @onchange@ attribute.
--
-- Example:
--
-- > div ! onchange "bar" $ "Hello."
--
-- Result:
--
-- > <div onchange="bar">Hello.</div>
--
onchange :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
onchange = attribute " onchange=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE onchange #-}                                                         -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onclick@ attribute.
--
-- Example:
--
-- > div ! onclick "bar" $ "Hello."
--
-- Result:
--
-- > <div onclick="bar">Hello.</div>
--
onclick :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onclick = attribute " onclick=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onclick #-}                                                          -- GenerateHtmlVariant.hs:239

-- | Combinator for the @ondblclick@ attribute.
--
-- Example:
--
-- > div ! ondblclick "bar" $ "Hello."
--
-- Result:
--
-- > <div ondblclick="bar">Hello.</div>
--
ondblclick :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
ondblclick = attribute " ondblclick=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE ondblclick #-}                                                       -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @onkeydown@ attribute.
--
-- Example:
--
-- > div ! onkeydown "bar" $ "Hello."
--
-- Result:
--
-- > <div onkeydown="bar">Hello.</div>
--
onkeydown :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
onkeydown = attribute " onkeydown=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE onkeydown #-}                                                        -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onkeypress@ attribute.
--
-- Example:
--
-- > div ! onkeypress "bar" $ "Hello."
--
-- Result:
--
-- > <div onkeypress="bar">Hello.</div>
--
onkeypress :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
onkeypress = attribute " onkeypress=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE onkeypress #-}                                                       -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onkeyup@ attribute.
--
-- Example:
--
-- > div ! onkeyup "bar" $ "Hello."
--
-- Result:
--
-- > <div onkeyup="bar">Hello.</div>
--
onkeyup :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onkeyup = attribute " onkeyup=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onkeyup #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @onmousedown@ attribute.
--
-- Example:
--
-- > div ! onmousedown "bar" $ "Hello."
--
-- Result:
--
-- > <div onmousedown="bar">Hello.</div>
--
onmousedown :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
onmousedown = attribute " onmousedown=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE onmousedown #-}                                                      -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onmousemove@ attribute.
--
-- Example:
--
-- > div ! onmousemove "bar" $ "Hello."
--
-- Result:
--
-- > <div onmousemove="bar">Hello.</div>
--
onmousemove :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
onmousemove = attribute " onmousemove=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE onmousemove #-}                                                      -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onmouseout@ attribute.
--
-- Example:
--
-- > div ! onmouseout "bar" $ "Hello."
--
-- Result:
--
-- > <div onmouseout="bar">Hello.</div>
--
onmouseout :: AttributeValue  -- ^ Attribute value.                             -- GenerateHtmlVariant.hs:236
           -> Attribute       -- ^ Resulting attribute.                         -- GenerateHtmlVariant.hs:237
onmouseout = attribute " onmouseout=\""                                         -- GenerateHtmlVariant.hs:238
{-# INLINE onmouseout #-}                                                       -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onmouseover@ attribute.
--
-- Example:
--
-- > div ! onmouseover "bar" $ "Hello."
--
-- Result:
--
-- > <div onmouseover="bar">Hello.</div>
--
onmouseover :: AttributeValue  -- ^ Attribute value.                            -- GenerateHtmlVariant.hs:236
            -> Attribute       -- ^ Resulting attribute.                        -- GenerateHtmlVariant.hs:237
onmouseover = attribute " onmouseover=\""                                       -- GenerateHtmlVariant.hs:238
{-# INLINE onmouseover #-}                                                      -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onmouseup@ attribute.
--
-- Example:
--
-- > div ! onmouseup "bar" $ "Hello."
--
-- Result:
--
-- > <div onmouseup="bar">Hello.</div>
--
onmouseup :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
onmouseup = attribute " onmouseup=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE onmouseup #-}                                                        -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onreset@ attribute.
--
-- Example:
--
-- > div ! onreset "bar" $ "Hello."
--
-- Result:
--
-- > <div onreset="bar">Hello.</div>
--
onreset :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
onreset = attribute " onreset=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE onreset #-}                                                          -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onselect@ attribute.
--
-- Example:
--
-- > div ! onselect "bar" $ "Hello."
--
-- Result:
--
-- > <div onselect="bar">Hello.</div>
--
onselect :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
onselect = attribute " onselect=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE onselect #-}                                                         -- GenerateHtmlVariant.hs:239

-- | Combinator for the @onsubmit@ attribute.
--
-- Example:
--
-- > div ! onsubmit "bar" $ "Hello."
--
-- Result:
--
-- > <div onsubmit="bar">Hello.</div>
--
onsubmit :: AttributeValue  -- ^ Attribute value.                               -- GenerateHtmlVariant.hs:236
         -> Attribute       -- ^ Resulting attribute.                           -- GenerateHtmlVariant.hs:237
onsubmit = attribute " onsubmit=\""                                             -- GenerateHtmlVariant.hs:238
{-# INLINE onsubmit #-}                                                         -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @profile@ attribute.
--
-- Example:
--
-- > div ! profile "bar" $ "Hello."
--
-- Result:
--
-- > <div profile="bar">Hello.</div>
--
profile :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
profile = attribute " profile=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE profile #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @rev@ attribute.
--
-- Example:
--
-- > div ! rev "bar" $ "Hello."
--
-- Result:
--
-- > <div rev="bar">Hello.</div>
--
rev :: AttributeValue  -- ^ Attribute value.                                    -- GenerateHtmlVariant.hs:236
    -> Attribute       -- ^ Resulting attribute.                                -- GenerateHtmlVariant.hs:237
rev = attribute " rev=\""                                                       -- GenerateHtmlVariant.hs:238
{-# INLINE rev #-}                                                              -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @rules@ attribute.
--
-- Example:
--
-- > div ! rules "bar" $ "Hello."
--
-- Result:
--
-- > <div rules="bar">Hello.</div>
--
rules :: AttributeValue  -- ^ Attribute value.                                  -- GenerateHtmlVariant.hs:236
      -> Attribute       -- ^ Resulting attribute.                              -- GenerateHtmlVariant.hs:237
rules = attribute " rules=\""                                                   -- GenerateHtmlVariant.hs:238
{-# INLINE rules #-}                                                            -- GenerateHtmlVariant.hs:239

-- | Combinator for the @scheme@ attribute.
--
-- Example:
--
-- > div ! scheme "bar" $ "Hello."
--
-- Result:
--
-- > <div scheme="bar">Hello.</div>
--
scheme :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
scheme = attribute " scheme=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE scheme #-}                                                           -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @standby@ attribute.
--
-- Example:
--
-- > div ! standby "bar" $ "Hello."
--
-- Result:
--
-- > <div standby="bar">Hello.</div>
--
standby :: AttributeValue  -- ^ Attribute value.                                -- GenerateHtmlVariant.hs:236
        -> Attribute       -- ^ Resulting attribute.                            -- GenerateHtmlVariant.hs:237
standby = attribute " standby=\""                                               -- GenerateHtmlVariant.hs:238
{-# INLINE standby #-}                                                          -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @valign@ attribute.
--
-- Example:
--
-- > div ! valign "bar" $ "Hello."
--
-- Result:
--
-- > <div valign="bar">Hello.</div>
--
valign :: AttributeValue  -- ^ Attribute value.                                 -- GenerateHtmlVariant.hs:236
       -> Attribute       -- ^ Resulting attribute.                             -- GenerateHtmlVariant.hs:237
valign = attribute " valign=\""                                                 -- GenerateHtmlVariant.hs:238
{-# INLINE valign #-}                                                           -- GenerateHtmlVariant.hs:239

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

-- | Combinator for the @valuetype@ attribute.
--
-- Example:
--
-- > div ! valuetype "bar" $ "Hello."
--
-- Result:
--
-- > <div valuetype="bar">Hello.</div>
--
valuetype :: AttributeValue  -- ^ Attribute value.                              -- GenerateHtmlVariant.hs:236
          -> Attribute       -- ^ Resulting attribute.                          -- GenerateHtmlVariant.hs:237
valuetype = attribute " valuetype=\""                                           -- GenerateHtmlVariant.hs:238
{-# INLINE valuetype #-}                                                        -- GenerateHtmlVariant.hs:239

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
