-- WARNING: This code was automatically generated. You should *never*
-- edit it directly. Instead, edit the files who generated this code,
-- you can find them in the @util/@ directory.

{-# LANGUAGE OverloadedStrings #-}                                              -- util/GenerateHtmlVariant.hs:50
-- | This module exports HTML combinators used to create documents.
--
module Text.Blaze.Html4.FrameSet                                                -- util/GenerateHtmlVariant.hs:121
    ( module Text.Blaze                                                         -- util/GenerateHtmlVariant.hs:122
    , html                                                                      -- util/GenerateHtmlVariant.hs:124
    , docType                                                                   -- util/GenerateHtmlVariant.hs:124
    , a                                                                         -- util/GenerateHtmlVariant.hs:124
    , abbr                                                                      -- util/GenerateHtmlVariant.hs:124
    , acronym                                                                   -- util/GenerateHtmlVariant.hs:124
    , address                                                                   -- util/GenerateHtmlVariant.hs:124
    , applet                                                                    -- util/GenerateHtmlVariant.hs:124
    , area                                                                      -- util/GenerateHtmlVariant.hs:124
    , b                                                                         -- util/GenerateHtmlVariant.hs:124
    , base                                                                      -- util/GenerateHtmlVariant.hs:124
    , basefont                                                                  -- util/GenerateHtmlVariant.hs:124
    , bdo                                                                       -- util/GenerateHtmlVariant.hs:124
    , big                                                                       -- util/GenerateHtmlVariant.hs:124
    , blockquote                                                                -- util/GenerateHtmlVariant.hs:124
    , body                                                                      -- util/GenerateHtmlVariant.hs:124
    , br                                                                        -- util/GenerateHtmlVariant.hs:124
    , button                                                                    -- util/GenerateHtmlVariant.hs:124
    , caption                                                                   -- util/GenerateHtmlVariant.hs:124
    , center                                                                    -- util/GenerateHtmlVariant.hs:124
    , cite                                                                      -- util/GenerateHtmlVariant.hs:124
    , code                                                                      -- util/GenerateHtmlVariant.hs:124
    , col                                                                       -- util/GenerateHtmlVariant.hs:124
    , colgroup                                                                  -- util/GenerateHtmlVariant.hs:124
    , dd                                                                        -- util/GenerateHtmlVariant.hs:124
    , del                                                                       -- util/GenerateHtmlVariant.hs:124
    , dfn                                                                       -- util/GenerateHtmlVariant.hs:124
    , dir                                                                       -- util/GenerateHtmlVariant.hs:124
    , div                                                                       -- util/GenerateHtmlVariant.hs:124
    , dl                                                                        -- util/GenerateHtmlVariant.hs:124
    , dt                                                                        -- util/GenerateHtmlVariant.hs:124
    , em                                                                        -- util/GenerateHtmlVariant.hs:124
    , fieldset                                                                  -- util/GenerateHtmlVariant.hs:124
    , font                                                                      -- util/GenerateHtmlVariant.hs:124
    , form                                                                      -- util/GenerateHtmlVariant.hs:124
    , frame                                                                     -- util/GenerateHtmlVariant.hs:124
    , frameset                                                                  -- util/GenerateHtmlVariant.hs:124
    , h6                                                                        -- util/GenerateHtmlVariant.hs:124
    , head                                                                      -- util/GenerateHtmlVariant.hs:124
    , hr                                                                        -- util/GenerateHtmlVariant.hs:124
    , htmlNoDocType                                                             -- util/GenerateHtmlVariant.hs:124
    , i                                                                         -- util/GenerateHtmlVariant.hs:124
    , iframe                                                                    -- util/GenerateHtmlVariant.hs:124
    , img                                                                       -- util/GenerateHtmlVariant.hs:124
    , input                                                                     -- util/GenerateHtmlVariant.hs:124
    , ins                                                                       -- util/GenerateHtmlVariant.hs:124
    , kbd                                                                       -- util/GenerateHtmlVariant.hs:124
    , label                                                                     -- util/GenerateHtmlVariant.hs:124
    , legend                                                                    -- util/GenerateHtmlVariant.hs:124
    , li                                                                        -- util/GenerateHtmlVariant.hs:124
    , link                                                                      -- util/GenerateHtmlVariant.hs:124
    , map                                                                       -- util/GenerateHtmlVariant.hs:124
    , menu                                                                      -- util/GenerateHtmlVariant.hs:124
    , meta                                                                      -- util/GenerateHtmlVariant.hs:124
    , noframes                                                                  -- util/GenerateHtmlVariant.hs:124
    , noscript                                                                  -- util/GenerateHtmlVariant.hs:124
    , object                                                                    -- util/GenerateHtmlVariant.hs:124
    , ol                                                                        -- util/GenerateHtmlVariant.hs:124
    , optgroup                                                                  -- util/GenerateHtmlVariant.hs:124
    , option                                                                    -- util/GenerateHtmlVariant.hs:124
    , p                                                                         -- util/GenerateHtmlVariant.hs:124
    , param                                                                     -- util/GenerateHtmlVariant.hs:124
    , pre                                                                       -- util/GenerateHtmlVariant.hs:124
    , q                                                                         -- util/GenerateHtmlVariant.hs:124
    , s                                                                         -- util/GenerateHtmlVariant.hs:124
    , samp                                                                      -- util/GenerateHtmlVariant.hs:124
    , script                                                                    -- util/GenerateHtmlVariant.hs:124
    , select                                                                    -- util/GenerateHtmlVariant.hs:124
    , small                                                                     -- util/GenerateHtmlVariant.hs:124
    , span                                                                      -- util/GenerateHtmlVariant.hs:124
    , strike                                                                    -- util/GenerateHtmlVariant.hs:124
    , strong                                                                    -- util/GenerateHtmlVariant.hs:124
    , style                                                                     -- util/GenerateHtmlVariant.hs:124
    , sub                                                                       -- util/GenerateHtmlVariant.hs:124
    , sup                                                                       -- util/GenerateHtmlVariant.hs:124
    , table                                                                     -- util/GenerateHtmlVariant.hs:124
    , tbody                                                                     -- util/GenerateHtmlVariant.hs:124
    , td                                                                        -- util/GenerateHtmlVariant.hs:124
    , textarea                                                                  -- util/GenerateHtmlVariant.hs:124
    , tfoot                                                                     -- util/GenerateHtmlVariant.hs:124
    , th                                                                        -- util/GenerateHtmlVariant.hs:124
    , thead                                                                     -- util/GenerateHtmlVariant.hs:124
    , title                                                                     -- util/GenerateHtmlVariant.hs:124
    , tr                                                                        -- util/GenerateHtmlVariant.hs:124
    , tt                                                                        -- util/GenerateHtmlVariant.hs:124
    , u                                                                         -- util/GenerateHtmlVariant.hs:124
    , ul                                                                        -- util/GenerateHtmlVariant.hs:124
    , var                                                                       -- util/GenerateHtmlVariant.hs:124
    ) where                                                                     -- util/GenerateHtmlVariant.hs:125

import Prelude ((>>), (.))                                                      -- util/GenerateHtmlVariant.hs:57
                                                                                -- util/GenerateHtmlVariant.hs:58
import Text.Blaze                                                               -- util/GenerateHtmlVariant.hs:59
import Text.Blaze.Internal                                                      -- util/GenerateHtmlVariant.hs:60
                                                                                -- util/GenerateHtmlVariant.hs:61
-- | Combinator for the @\<html>@ element. This combinator will also
-- insert the correct doctype.
--
-- Example:
--
-- > html $ span $ text "foo"
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
-- >     "http://www.w3.org/TR/html4/frameset.dtd">
-- > <html><span>foo</span></html>
--
html :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:162
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:163
html inner = docType >> htmlNoDocType inner                                     -- util/GenerateHtmlVariant.hs:164
{-# INLINE html #-}                                                             -- util/GenerateHtmlVariant.hs:165

-- | Combinator for the document type. This should be placed at the top
-- of every HTML page.
--
-- Example:
--
-- > docType
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
-- >     "http://www.w3.org/TR/html4/frameset.dtd">
--
docType :: Html  -- ^ The document type HTML.                                   -- util/GenerateHtmlVariant.hs:141
docType = preEscapedText "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"\n    \"http://www.w3.org/TR/html4/frameset.dtd\">\n" -- util/GenerateHtmlVariant.hs:142
{-# INLINE docType #-}                                                          -- util/GenerateHtmlVariant.hs:143

-- | Combinator for the @\<a>@ element.
--
-- Example:
--
-- > a $ span $ text "foo"
--
-- Result:
--
-- > <a><span>foo</span></a>
--
a :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
a = Parent "<a" "</a>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE a #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<abbr>@ element.
--
-- Example:
--
-- > abbr $ span $ text "foo"
--
-- Result:
--
-- > <abbr><span>foo</span></abbr>
--
abbr :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
abbr = Parent "<abbr" "</abbr>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE abbr #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<acronym>@ element.
--
-- Example:
--
-- > acronym $ span $ text "foo"
--
-- Result:
--
-- > <acronym><span>foo</span></acronym>
--
acronym :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:182
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:183
acronym = Parent "<acronym" "</acronym>"                                        -- util/GenerateHtmlVariant.hs:184
{-# INLINE acronym #-}                                                          -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<address>@ element.
--
-- Example:
--
-- > address $ span $ text "foo"
--
-- Result:
--
-- > <address><span>foo</span></address>
--
address :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:182
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:183
address = Parent "<address" "</address>"                                        -- util/GenerateHtmlVariant.hs:184
{-# INLINE address #-}                                                          -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<applet>@ element.
--
-- Example:
--
-- > applet $ span $ text "foo"
--
-- Result:
--
-- > <applet><span>foo</span></applet>
--
applet :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
applet = Parent "<applet" "</applet>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE applet #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<area>@ element.
--
-- Example:
--
-- > area
--
-- Result:
--
-- > <area>
--
area :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:227
area = Leaf "<area" ">"                                                         -- util/GenerateHtmlVariant.hs:228
{-# INLINE area #-}                                                             -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<b>@ element.
--
-- Example:
--
-- > b $ span $ text "foo"
--
-- Result:
--
-- > <b><span>foo</span></b>
--
b :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
b = Parent "<b" "</b>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE b #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<base>@ element.
--
-- Example:
--
-- > base
--
-- Result:
--
-- > <base>
--
base :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:227
base = Leaf "<base" ">"                                                         -- util/GenerateHtmlVariant.hs:228
{-# INLINE base #-}                                                             -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<basefont>@ element.
--
-- Example:
--
-- > basefont
--
-- Result:
--
-- > <basefont>
--
basefont :: Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:227
basefont = Leaf "<basefont" ">"                                                 -- util/GenerateHtmlVariant.hs:228
{-# INLINE basefont #-}                                                         -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<bdo>@ element.
--
-- Example:
--
-- > bdo $ span $ text "foo"
--
-- Result:
--
-- > <bdo><span>foo</span></bdo>
--
bdo :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
bdo = Parent "<bdo" "</bdo>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE bdo #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<big>@ element.
--
-- Example:
--
-- > big $ span $ text "foo"
--
-- Result:
--
-- > <big><span>foo</span></big>
--
big :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
big = Parent "<big" "</big>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE big #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<blockquote>@ element.
--
-- Example:
--
-- > blockquote $ span $ text "foo"
--
-- Result:
--
-- > <blockquote><span>foo</span></blockquote>
--
blockquote :: Html  -- ^ Inner HTML.                                            -- util/GenerateHtmlVariant.hs:182
           -> Html  -- ^ Resulting HTML.                                        -- util/GenerateHtmlVariant.hs:183
blockquote = Parent "<blockquote" "</blockquote>"                               -- util/GenerateHtmlVariant.hs:184
{-# INLINE blockquote #-}                                                       -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<body>@ element.
--
-- Example:
--
-- > body $ span $ text "foo"
--
-- Result:
--
-- > <body><span>foo</span></body>
--
body :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
body = Parent "<body" "</body>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE body #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<br>@ element.
--
-- Example:
--
-- > br
--
-- Result:
--
-- > <br>
--
br :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:227
br = Leaf "<br" ">"                                                             -- util/GenerateHtmlVariant.hs:228
{-# INLINE br #-}                                                               -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<button>@ element.
--
-- Example:
--
-- > button $ span $ text "foo"
--
-- Result:
--
-- > <button><span>foo</span></button>
--
button :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
button = Parent "<button" "</button>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE button #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<caption>@ element.
--
-- Example:
--
-- > caption $ span $ text "foo"
--
-- Result:
--
-- > <caption><span>foo</span></caption>
--
caption :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:182
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:183
caption = Parent "<caption" "</caption>"                                        -- util/GenerateHtmlVariant.hs:184
{-# INLINE caption #-}                                                          -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<center>@ element.
--
-- Example:
--
-- > center $ span $ text "foo"
--
-- Result:
--
-- > <center><span>foo</span></center>
--
center :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
center = Parent "<center" "</center>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE center #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<cite>@ element.
--
-- Example:
--
-- > cite $ span $ text "foo"
--
-- Result:
--
-- > <cite><span>foo</span></cite>
--
cite :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
cite = Parent "<cite" "</cite>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE cite #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<code>@ element.
--
-- Example:
--
-- > code $ span $ text "foo"
--
-- Result:
--
-- > <code><span>foo</span></code>
--
code :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
code = Parent "<code" "</code>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE code #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<col>@ element.
--
-- Example:
--
-- > col
--
-- Result:
--
-- > <col>
--
col :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:227
col = Leaf "<col" ">"                                                           -- util/GenerateHtmlVariant.hs:228
{-# INLINE col #-}                                                              -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<colgroup>@ element.
--
-- Example:
--
-- > colgroup $ span $ text "foo"
--
-- Result:
--
-- > <colgroup><span>foo</span></colgroup>
--
colgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
colgroup = Parent "<colgroup" "</colgroup>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE colgroup #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<dd>@ element.
--
-- Example:
--
-- > dd $ span $ text "foo"
--
-- Result:
--
-- > <dd><span>foo</span></dd>
--
dd :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
dd = Parent "<dd" "</dd>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE dd #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<del>@ element.
--
-- Example:
--
-- > del $ span $ text "foo"
--
-- Result:
--
-- > <del><span>foo</span></del>
--
del :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
del = Parent "<del" "</del>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE del #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<dfn>@ element.
--
-- Example:
--
-- > dfn $ span $ text "foo"
--
-- Result:
--
-- > <dfn><span>foo</span></dfn>
--
dfn :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
dfn = Parent "<dfn" "</dfn>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE dfn #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<dir>@ element.
--
-- Example:
--
-- > dir $ span $ text "foo"
--
-- Result:
--
-- > <dir><span>foo</span></dir>
--
dir :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
dir = Parent "<dir" "</dir>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE dir #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<div>@ element.
--
-- Example:
--
-- > div $ span $ text "foo"
--
-- Result:
--
-- > <div><span>foo</span></div>
--
div :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
div = Parent "<div" "</div>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE div #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<dl>@ element.
--
-- Example:
--
-- > dl $ span $ text "foo"
--
-- Result:
--
-- > <dl><span>foo</span></dl>
--
dl :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
dl = Parent "<dl" "</dl>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE dl #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<dt>@ element.
--
-- Example:
--
-- > dt $ span $ text "foo"
--
-- Result:
--
-- > <dt><span>foo</span></dt>
--
dt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
dt = Parent "<dt" "</dt>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE dt #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<em>@ element.
--
-- Example:
--
-- > em $ span $ text "foo"
--
-- Result:
--
-- > <em><span>foo</span></em>
--
em :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
em = Parent "<em" "</em>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE em #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<fieldset>@ element.
--
-- Example:
--
-- > fieldset $ span $ text "foo"
--
-- Result:
--
-- > <fieldset><span>foo</span></fieldset>
--
fieldset :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
fieldset = Parent "<fieldset" "</fieldset>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE fieldset #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<font>@ element.
--
-- Example:
--
-- > font $ span $ text "foo"
--
-- Result:
--
-- > <font><span>foo</span></font>
--
font :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
font = Parent "<font" "</font>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE font #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<form>@ element.
--
-- Example:
--
-- > form $ span $ text "foo"
--
-- Result:
--
-- > <form><span>foo</span></form>
--
form :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
form = Parent "<form" "</form>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE form #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<frame>@ element.
--
-- Example:
--
-- > frame
--
-- Result:
--
-- > <frame>
--
frame :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:227
frame = Leaf "<frame" ">"                                                       -- util/GenerateHtmlVariant.hs:228
{-# INLINE frame #-}                                                            -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<frameset>@ element.
--
-- Example:
--
-- > frameset $ span $ text "foo"
--
-- Result:
--
-- > <frameset><span>foo</span></frameset>
--
frameset :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
frameset = Parent "<frameset" "</frameset>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE frameset #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<h6>@ element.
--
-- Example:
--
-- > h6 $ span $ text "foo"
--
-- Result:
--
-- > <h6><span>foo</span></h6>
--
h6 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
h6 = Parent "<h6" "</h6>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE h6 #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<head>@ element.
--
-- Example:
--
-- > head $ span $ text "foo"
--
-- Result:
--
-- > <head><span>foo</span></head>
--
head :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
head = Parent "<head" "</head>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE head #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<hr>@ element.
--
-- Example:
--
-- > hr
--
-- Result:
--
-- > <hr>
--
hr :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:227
hr = Leaf "<hr" ">"                                                             -- util/GenerateHtmlVariant.hs:228
{-# INLINE hr #-}                                                               -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<html>@ element.
--
-- Example:
--
-- > htmlNoDocType $ span $ text "foo"
--
-- Result:
--
-- > <html><span>foo</span></html>
--
htmlNoDocType :: Html  -- ^ Inner HTML.                                         -- util/GenerateHtmlVariant.hs:182
              -> Html  -- ^ Resulting HTML.                                     -- util/GenerateHtmlVariant.hs:183
htmlNoDocType = Parent "<html" "</html>"                                        -- util/GenerateHtmlVariant.hs:184
{-# INLINE htmlNoDocType #-}                                                    -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<i>@ element.
--
-- Example:
--
-- > i $ span $ text "foo"
--
-- Result:
--
-- > <i><span>foo</span></i>
--
i :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
i = Parent "<i" "</i>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE i #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<iframe>@ element.
--
-- Example:
--
-- > iframe $ span $ text "foo"
--
-- Result:
--
-- > <iframe><span>foo</span></iframe>
--
iframe :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
iframe = Parent "<iframe" "</iframe>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE iframe #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<img>@ element.
--
-- Example:
--
-- > img
--
-- Result:
--
-- > <img>
--
img :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:227
img = Leaf "<img" ">"                                                           -- util/GenerateHtmlVariant.hs:228
{-# INLINE img #-}                                                              -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<input>@ element.
--
-- Example:
--
-- > input
--
-- Result:
--
-- > <input>
--
input :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:227
input = Leaf "<input" ">"                                                       -- util/GenerateHtmlVariant.hs:228
{-# INLINE input #-}                                                            -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<ins>@ element.
--
-- Example:
--
-- > ins $ span $ text "foo"
--
-- Result:
--
-- > <ins><span>foo</span></ins>
--
ins :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
ins = Parent "<ins" "</ins>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE ins #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<kbd>@ element.
--
-- Example:
--
-- > kbd $ span $ text "foo"
--
-- Result:
--
-- > <kbd><span>foo</span></kbd>
--
kbd :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
kbd = Parent "<kbd" "</kbd>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE kbd #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<label>@ element.
--
-- Example:
--
-- > label $ span $ text "foo"
--
-- Result:
--
-- > <label><span>foo</span></label>
--
label :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
label = Parent "<label" "</label>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE label #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<legend>@ element.
--
-- Example:
--
-- > legend $ span $ text "foo"
--
-- Result:
--
-- > <legend><span>foo</span></legend>
--
legend :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
legend = Parent "<legend" "</legend>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE legend #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<li>@ element.
--
-- Example:
--
-- > li $ span $ text "foo"
--
-- Result:
--
-- > <li><span>foo</span></li>
--
li :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
li = Parent "<li" "</li>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE li #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<link>@ element.
--
-- Example:
--
-- > link
--
-- Result:
--
-- > <link>
--
link :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:227
link = Leaf "<link" ">"                                                         -- util/GenerateHtmlVariant.hs:228
{-# INLINE link #-}                                                             -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<map>@ element.
--
-- Example:
--
-- > map $ span $ text "foo"
--
-- Result:
--
-- > <map><span>foo</span></map>
--
map :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
map = Parent "<map" "</map>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE map #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<menu>@ element.
--
-- Example:
--
-- > menu $ span $ text "foo"
--
-- Result:
--
-- > <menu><span>foo</span></menu>
--
menu :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
menu = Parent "<menu" "</menu>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE menu #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<meta>@ element.
--
-- Example:
--
-- > meta
--
-- Result:
--
-- > <meta>
--
meta :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:227
meta = Leaf "<meta" ">"                                                         -- util/GenerateHtmlVariant.hs:228
{-# INLINE meta #-}                                                             -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<noframes>@ element.
--
-- Example:
--
-- > noframes $ span $ text "foo"
--
-- Result:
--
-- > <noframes><span>foo</span></noframes>
--
noframes :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
noframes = Parent "<noframes" "</noframes>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE noframes #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<noscript>@ element.
--
-- Example:
--
-- > noscript $ span $ text "foo"
--
-- Result:
--
-- > <noscript><span>foo</span></noscript>
--
noscript :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
noscript = Parent "<noscript" "</noscript>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE noscript #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<object>@ element.
--
-- Example:
--
-- > object $ span $ text "foo"
--
-- Result:
--
-- > <object><span>foo</span></object>
--
object :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
object = Parent "<object" "</object>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE object #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<ol>@ element.
--
-- Example:
--
-- > ol $ span $ text "foo"
--
-- Result:
--
-- > <ol><span>foo</span></ol>
--
ol :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
ol = Parent "<ol" "</ol>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE ol #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<optgroup>@ element.
--
-- Example:
--
-- > optgroup $ span $ text "foo"
--
-- Result:
--
-- > <optgroup><span>foo</span></optgroup>
--
optgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
optgroup = Parent "<optgroup" "</optgroup>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE optgroup #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<option>@ element.
--
-- Example:
--
-- > option $ span $ text "foo"
--
-- Result:
--
-- > <option><span>foo</span></option>
--
option :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
option = Parent "<option" "</option>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE option #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<p>@ element.
--
-- Example:
--
-- > p $ span $ text "foo"
--
-- Result:
--
-- > <p><span>foo</span></p>
--
p :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
p = Parent "<p" "</p>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE p #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<param>@ element.
--
-- Example:
--
-- > param
--
-- Result:
--
-- > <param>
--
param :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:227
param = Leaf "<param" ">"                                                       -- util/GenerateHtmlVariant.hs:228
{-# INLINE param #-}                                                            -- util/GenerateHtmlVariant.hs:229

-- | Combinator for the @\<pre>@ element.
--
-- Example:
--
-- > pre $ span $ text "foo"
--
-- Result:
--
-- > <pre><span>foo</span></pre>
--
pre :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
pre = Parent "<pre" "</pre>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE pre #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<q>@ element.
--
-- Example:
--
-- > q $ span $ text "foo"
--
-- Result:
--
-- > <q><span>foo</span></q>
--
q :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
q = Parent "<q" "</q>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE q #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<s>@ element.
--
-- Example:
--
-- > s $ span $ text "foo"
--
-- Result:
--
-- > <s><span>foo</span></s>
--
s :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
s = Parent "<s" "</s>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE s #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<samp>@ element.
--
-- Example:
--
-- > samp $ span $ text "foo"
--
-- Result:
--
-- > <samp><span>foo</span></samp>
--
samp :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
samp = Parent "<samp" "</samp>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE samp #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<script>@ element.
--
-- Example:
--
-- > script $ span $ text "foo"
--
-- Result:
--
-- > <script><span>foo</span></script>
--
script :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
script = Parent "<script" "</script>" . external                                -- util/GenerateHtmlVariant.hs:184
{-# INLINE script #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<select>@ element.
--
-- Example:
--
-- > select $ span $ text "foo"
--
-- Result:
--
-- > <select><span>foo</span></select>
--
select :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
select = Parent "<select" "</select>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE select #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<small>@ element.
--
-- Example:
--
-- > small $ span $ text "foo"
--
-- Result:
--
-- > <small><span>foo</span></small>
--
small :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
small = Parent "<small" "</small>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE small #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<span>@ element.
--
-- Example:
--
-- > span $ span $ text "foo"
--
-- Result:
--
-- > <span><span>foo</span></span>
--
span :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:182
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:183
span = Parent "<span" "</span>"                                                 -- util/GenerateHtmlVariant.hs:184
{-# INLINE span #-}                                                             -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<strike>@ element.
--
-- Example:
--
-- > strike $ span $ text "foo"
--
-- Result:
--
-- > <strike><span>foo</span></strike>
--
strike :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
strike = Parent "<strike" "</strike>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE strike #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<strong>@ element.
--
-- Example:
--
-- > strong $ span $ text "foo"
--
-- Result:
--
-- > <strong><span>foo</span></strong>
--
strong :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:182
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:183
strong = Parent "<strong" "</strong>"                                           -- util/GenerateHtmlVariant.hs:184
{-# INLINE strong #-}                                                           -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<style>@ element.
--
-- Example:
--
-- > style $ span $ text "foo"
--
-- Result:
--
-- > <style><span>foo</span></style>
--
style :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
style = Parent "<style" "</style>" . external                                   -- util/GenerateHtmlVariant.hs:184
{-# INLINE style #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<sub>@ element.
--
-- Example:
--
-- > sub $ span $ text "foo"
--
-- Result:
--
-- > <sub><span>foo</span></sub>
--
sub :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
sub = Parent "<sub" "</sub>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE sub #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<sup>@ element.
--
-- Example:
--
-- > sup $ span $ text "foo"
--
-- Result:
--
-- > <sup><span>foo</span></sup>
--
sup :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
sup = Parent "<sup" "</sup>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE sup #-}                                                              -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<table>@ element.
--
-- Example:
--
-- > table $ span $ text "foo"
--
-- Result:
--
-- > <table><span>foo</span></table>
--
table :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
table = Parent "<table" "</table>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE table #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<tbody>@ element.
--
-- Example:
--
-- > tbody $ span $ text "foo"
--
-- Result:
--
-- > <tbody><span>foo</span></tbody>
--
tbody :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
tbody = Parent "<tbody" "</tbody>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE tbody #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<td>@ element.
--
-- Example:
--
-- > td $ span $ text "foo"
--
-- Result:
--
-- > <td><span>foo</span></td>
--
td :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
td = Parent "<td" "</td>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE td #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<textarea>@ element.
--
-- Example:
--
-- > textarea $ span $ text "foo"
--
-- Result:
--
-- > <textarea><span>foo</span></textarea>
--
textarea :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:182
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:183
textarea = Parent "<textarea" "</textarea>"                                     -- util/GenerateHtmlVariant.hs:184
{-# INLINE textarea #-}                                                         -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<tfoot>@ element.
--
-- Example:
--
-- > tfoot $ span $ text "foo"
--
-- Result:
--
-- > <tfoot><span>foo</span></tfoot>
--
tfoot :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
tfoot = Parent "<tfoot" "</tfoot>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE tfoot #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<th>@ element.
--
-- Example:
--
-- > th $ span $ text "foo"
--
-- Result:
--
-- > <th><span>foo</span></th>
--
th :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
th = Parent "<th" "</th>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE th #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<thead>@ element.
--
-- Example:
--
-- > thead $ span $ text "foo"
--
-- Result:
--
-- > <thead><span>foo</span></thead>
--
thead :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
thead = Parent "<thead" "</thead>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE thead #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<title>@ element.
--
-- Example:
--
-- > title $ span $ text "foo"
--
-- Result:
--
-- > <title><span>foo</span></title>
--
title :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:182
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:183
title = Parent "<title" "</title>"                                              -- util/GenerateHtmlVariant.hs:184
{-# INLINE title #-}                                                            -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<tr>@ element.
--
-- Example:
--
-- > tr $ span $ text "foo"
--
-- Result:
--
-- > <tr><span>foo</span></tr>
--
tr :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
tr = Parent "<tr" "</tr>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE tr #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<tt>@ element.
--
-- Example:
--
-- > tt $ span $ text "foo"
--
-- Result:
--
-- > <tt><span>foo</span></tt>
--
tt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
tt = Parent "<tt" "</tt>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE tt #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<u>@ element.
--
-- Example:
--
-- > u $ span $ text "foo"
--
-- Result:
--
-- > <u><span>foo</span></u>
--
u :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:182
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:183
u = Parent "<u" "</u>"                                                          -- util/GenerateHtmlVariant.hs:184
{-# INLINE u #-}                                                                -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<ul>@ element.
--
-- Example:
--
-- > ul $ span $ text "foo"
--
-- Result:
--
-- > <ul><span>foo</span></ul>
--
ul :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:182
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:183
ul = Parent "<ul" "</ul>"                                                       -- util/GenerateHtmlVariant.hs:184
{-# INLINE ul #-}                                                               -- util/GenerateHtmlVariant.hs:186

-- | Combinator for the @\<var>@ element.
--
-- Example:
--
-- > var $ span $ text "foo"
--
-- Result:
--
-- > <var><span>foo</span></var>
--
var :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:182
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:183
var = Parent "<var" "</var>"                                                    -- util/GenerateHtmlVariant.hs:184
{-# INLINE var #-}                                                              -- util/GenerateHtmlVariant.hs:186
