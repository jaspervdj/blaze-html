-- WARNING: This code was automatically generated. You should *never*
-- edit it directly. Instead, edit the files who generated this code,
-- you can find them in the @util/@ directory.

{-# LANGUAGE OverloadedStrings #-}                                              -- util/GenerateHtmlVariant.hs:49
-- | This module exports HTML combinators used to create documents.
--
module Text.Blaze.Html4.Strict                                                  -- util/GenerateHtmlVariant.hs:120
    ( module Text.Blaze                                                         -- util/GenerateHtmlVariant.hs:121
    , html                                                                      -- util/GenerateHtmlVariant.hs:123
    , docType                                                                   -- util/GenerateHtmlVariant.hs:123
    , a                                                                         -- util/GenerateHtmlVariant.hs:123
    , abbr                                                                      -- util/GenerateHtmlVariant.hs:123
    , acronym                                                                   -- util/GenerateHtmlVariant.hs:123
    , address                                                                   -- util/GenerateHtmlVariant.hs:123
    , area                                                                      -- util/GenerateHtmlVariant.hs:123
    , b                                                                         -- util/GenerateHtmlVariant.hs:123
    , bdo                                                                       -- util/GenerateHtmlVariant.hs:123
    , big                                                                       -- util/GenerateHtmlVariant.hs:123
    , blockquote                                                                -- util/GenerateHtmlVariant.hs:123
    , body                                                                      -- util/GenerateHtmlVariant.hs:123
    , br                                                                        -- util/GenerateHtmlVariant.hs:123
    , button                                                                    -- util/GenerateHtmlVariant.hs:123
    , caption                                                                   -- util/GenerateHtmlVariant.hs:123
    , cite                                                                      -- util/GenerateHtmlVariant.hs:123
    , code                                                                      -- util/GenerateHtmlVariant.hs:123
    , col                                                                       -- util/GenerateHtmlVariant.hs:123
    , colgroup                                                                  -- util/GenerateHtmlVariant.hs:123
    , dd                                                                        -- util/GenerateHtmlVariant.hs:123
    , del                                                                       -- util/GenerateHtmlVariant.hs:123
    , dfn                                                                       -- util/GenerateHtmlVariant.hs:123
    , div                                                                       -- util/GenerateHtmlVariant.hs:123
    , dl                                                                        -- util/GenerateHtmlVariant.hs:123
    , dt                                                                        -- util/GenerateHtmlVariant.hs:123
    , em                                                                        -- util/GenerateHtmlVariant.hs:123
    , fieldset                                                                  -- util/GenerateHtmlVariant.hs:123
    , form                                                                      -- util/GenerateHtmlVariant.hs:123
    , h1                                                                        -- util/GenerateHtmlVariant.hs:123
    , h2                                                                        -- util/GenerateHtmlVariant.hs:123
    , h3                                                                        -- util/GenerateHtmlVariant.hs:123
    , h4                                                                        -- util/GenerateHtmlVariant.hs:123
    , h5                                                                        -- util/GenerateHtmlVariant.hs:123
    , h6                                                                        -- util/GenerateHtmlVariant.hs:123
    , head                                                                      -- util/GenerateHtmlVariant.hs:123
    , hr                                                                        -- util/GenerateHtmlVariant.hs:123
    , htmlNoDocType                                                             -- util/GenerateHtmlVariant.hs:123
    , i                                                                         -- util/GenerateHtmlVariant.hs:123
    , img                                                                       -- util/GenerateHtmlVariant.hs:123
    , input                                                                     -- util/GenerateHtmlVariant.hs:123
    , ins                                                                       -- util/GenerateHtmlVariant.hs:123
    , kbd                                                                       -- util/GenerateHtmlVariant.hs:123
    , label                                                                     -- util/GenerateHtmlVariant.hs:123
    , legend                                                                    -- util/GenerateHtmlVariant.hs:123
    , li                                                                        -- util/GenerateHtmlVariant.hs:123
    , link                                                                      -- util/GenerateHtmlVariant.hs:123
    , map                                                                       -- util/GenerateHtmlVariant.hs:123
    , meta                                                                      -- util/GenerateHtmlVariant.hs:123
    , noscript                                                                  -- util/GenerateHtmlVariant.hs:123
    , object                                                                    -- util/GenerateHtmlVariant.hs:123
    , ol                                                                        -- util/GenerateHtmlVariant.hs:123
    , optgroup                                                                  -- util/GenerateHtmlVariant.hs:123
    , option                                                                    -- util/GenerateHtmlVariant.hs:123
    , p                                                                         -- util/GenerateHtmlVariant.hs:123
    , param                                                                     -- util/GenerateHtmlVariant.hs:123
    , pre                                                                       -- util/GenerateHtmlVariant.hs:123
    , q                                                                         -- util/GenerateHtmlVariant.hs:123
    , samp                                                                      -- util/GenerateHtmlVariant.hs:123
    , script                                                                    -- util/GenerateHtmlVariant.hs:123
    , select                                                                    -- util/GenerateHtmlVariant.hs:123
    , small                                                                     -- util/GenerateHtmlVariant.hs:123
    , span                                                                      -- util/GenerateHtmlVariant.hs:123
    , strong                                                                    -- util/GenerateHtmlVariant.hs:123
    , style                                                                     -- util/GenerateHtmlVariant.hs:123
    , sub                                                                       -- util/GenerateHtmlVariant.hs:123
    , sup                                                                       -- util/GenerateHtmlVariant.hs:123
    , table                                                                     -- util/GenerateHtmlVariant.hs:123
    , tbody                                                                     -- util/GenerateHtmlVariant.hs:123
    , td                                                                        -- util/GenerateHtmlVariant.hs:123
    , textarea                                                                  -- util/GenerateHtmlVariant.hs:123
    , tfoot                                                                     -- util/GenerateHtmlVariant.hs:123
    , th                                                                        -- util/GenerateHtmlVariant.hs:123
    , thead                                                                     -- util/GenerateHtmlVariant.hs:123
    , title                                                                     -- util/GenerateHtmlVariant.hs:123
    , tr                                                                        -- util/GenerateHtmlVariant.hs:123
    , tt                                                                        -- util/GenerateHtmlVariant.hs:123
    , ul                                                                        -- util/GenerateHtmlVariant.hs:123
    , var                                                                       -- util/GenerateHtmlVariant.hs:123
    ) where                                                                     -- util/GenerateHtmlVariant.hs:124

import Prelude ((>>))                                                           -- util/GenerateHtmlVariant.hs:56
                                                                                -- util/GenerateHtmlVariant.hs:57
import Text.Blaze                                                               -- util/GenerateHtmlVariant.hs:58
import Text.Blaze.Internal                                                      -- util/GenerateHtmlVariant.hs:59
                                                                                -- util/GenerateHtmlVariant.hs:60
-- | Combinator for the @\<html>@ element. This combinator will also
-- insert the correct doctype.
--
-- Example:
--
-- > html $ span $ text "foo"
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
-- >     "http://www.w3.org/TR/html4/strict.dtd">
-- > <html><span>foo</span></html>
--
html :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:161
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:162
html inner = docType >> htmlNoDocType inner                                     -- util/GenerateHtmlVariant.hs:163
{-# INLINE html #-}                                                             -- util/GenerateHtmlVariant.hs:164

-- | Combinator for the document type. This should be placed at the top
-- of every HTML page.
--
-- Example:
--
-- > docType
--
-- Result:
--
-- > <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
-- >     "http://www.w3.org/TR/html4/strict.dtd">
--
docType :: Html  -- ^ The document type HTML.                                   -- util/GenerateHtmlVariant.hs:140
docType = preEscapedText "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n    \"http://www.w3.org/TR/html4/strict.dtd\">\n" -- util/GenerateHtmlVariant.hs:141
{-# INLINE docType #-}                                                          -- util/GenerateHtmlVariant.hs:142

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
a :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:181
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:182
a = Parent "<a" "</a>"                                                          -- util/GenerateHtmlVariant.hs:183
{-# INLINE a #-}                                                                -- util/GenerateHtmlVariant.hs:184

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
abbr :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
abbr = Parent "<abbr" "</abbr>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE abbr #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
acronym :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:181
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:182
acronym = Parent "<acronym" "</acronym>"                                        -- util/GenerateHtmlVariant.hs:183
{-# INLINE acronym #-}                                                          -- util/GenerateHtmlVariant.hs:184

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
address :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:181
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:182
address = Parent "<address" "</address>"                                        -- util/GenerateHtmlVariant.hs:183
{-# INLINE address #-}                                                          -- util/GenerateHtmlVariant.hs:184

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
area :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:224
area = Open "<area" ">"                                                         -- util/GenerateHtmlVariant.hs:225
{-# INLINE area #-}                                                             -- util/GenerateHtmlVariant.hs:226

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
b :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:181
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:182
b = Parent "<b" "</b>"                                                          -- util/GenerateHtmlVariant.hs:183
{-# INLINE b #-}                                                                -- util/GenerateHtmlVariant.hs:184

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
bdo :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
bdo = Parent "<bdo" "</bdo>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE bdo #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
big :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
big = Parent "<big" "</big>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE big #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
blockquote :: Html  -- ^ Inner HTML.                                            -- util/GenerateHtmlVariant.hs:181
           -> Html  -- ^ Resulting HTML.                                        -- util/GenerateHtmlVariant.hs:182
blockquote = Parent "<blockquote" "</blockquote>"                               -- util/GenerateHtmlVariant.hs:183
{-# INLINE blockquote #-}                                                       -- util/GenerateHtmlVariant.hs:184

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
body :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
body = Parent "<body" "</body>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE body #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
br :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:224
br = Open "<br" ">"                                                             -- util/GenerateHtmlVariant.hs:225
{-# INLINE br #-}                                                               -- util/GenerateHtmlVariant.hs:226

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
button :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
button = Parent "<button" "</button>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE button #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
caption :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:181
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:182
caption = Parent "<caption" "</caption>"                                        -- util/GenerateHtmlVariant.hs:183
{-# INLINE caption #-}                                                          -- util/GenerateHtmlVariant.hs:184

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
cite :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
cite = Parent "<cite" "</cite>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE cite #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
code :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
code = Parent "<code" "</code>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE code #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
col :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:224
col = Open "<col" ">"                                                           -- util/GenerateHtmlVariant.hs:225
{-# INLINE col #-}                                                              -- util/GenerateHtmlVariant.hs:226

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
colgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:181
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:182
colgroup = Parent "<colgroup" "</colgroup>"                                     -- util/GenerateHtmlVariant.hs:183
{-# INLINE colgroup #-}                                                         -- util/GenerateHtmlVariant.hs:184

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
dd :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
dd = Parent "<dd" "</dd>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE dd #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
del :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
del = Parent "<del" "</del>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE del #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
dfn :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
dfn = Parent "<dfn" "</dfn>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE dfn #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
div :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
div = Parent "<div" "</div>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE div #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
dl :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
dl = Parent "<dl" "</dl>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE dl #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
dt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
dt = Parent "<dt" "</dt>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE dt #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
em :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
em = Parent "<em" "</em>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE em #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
fieldset :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:181
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:182
fieldset = Parent "<fieldset" "</fieldset>"                                     -- util/GenerateHtmlVariant.hs:183
{-# INLINE fieldset #-}                                                         -- util/GenerateHtmlVariant.hs:184

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
form :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
form = Parent "<form" "</form>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE form #-}                                                             -- util/GenerateHtmlVariant.hs:184

-- | Combinator for the @\<h1>@ element.
--
-- Example:
--
-- > h1 $ span $ text "foo"
--
-- Result:
--
-- > <h1><span>foo</span></h1>
--
h1 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h1 = Parent "<h1" "</h1>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h1 #-}                                                               -- util/GenerateHtmlVariant.hs:184

-- | Combinator for the @\<h2>@ element.
--
-- Example:
--
-- > h2 $ span $ text "foo"
--
-- Result:
--
-- > <h2><span>foo</span></h2>
--
h2 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h2 = Parent "<h2" "</h2>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h2 #-}                                                               -- util/GenerateHtmlVariant.hs:184

-- | Combinator for the @\<h3>@ element.
--
-- Example:
--
-- > h3 $ span $ text "foo"
--
-- Result:
--
-- > <h3><span>foo</span></h3>
--
h3 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h3 = Parent "<h3" "</h3>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h3 #-}                                                               -- util/GenerateHtmlVariant.hs:184

-- | Combinator for the @\<h4>@ element.
--
-- Example:
--
-- > h4 $ span $ text "foo"
--
-- Result:
--
-- > <h4><span>foo</span></h4>
--
h4 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h4 = Parent "<h4" "</h4>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h4 #-}                                                               -- util/GenerateHtmlVariant.hs:184

-- | Combinator for the @\<h5>@ element.
--
-- Example:
--
-- > h5 $ span $ text "foo"
--
-- Result:
--
-- > <h5><span>foo</span></h5>
--
h5 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h5 = Parent "<h5" "</h5>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h5 #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
h6 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
h6 = Parent "<h6" "</h6>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE h6 #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
head :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
head = Parent "<head" "</head>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE head #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
hr :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:224
hr = Open "<hr" ">"                                                             -- util/GenerateHtmlVariant.hs:225
{-# INLINE hr #-}                                                               -- util/GenerateHtmlVariant.hs:226

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
htmlNoDocType :: Html  -- ^ Inner HTML.                                         -- util/GenerateHtmlVariant.hs:181
              -> Html  -- ^ Resulting HTML.                                     -- util/GenerateHtmlVariant.hs:182
htmlNoDocType = Parent "<html" "</html>"                                        -- util/GenerateHtmlVariant.hs:183
{-# INLINE htmlNoDocType #-}                                                    -- util/GenerateHtmlVariant.hs:184

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
i :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:181
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:182
i = Parent "<i" "</i>"                                                          -- util/GenerateHtmlVariant.hs:183
{-# INLINE i #-}                                                                -- util/GenerateHtmlVariant.hs:184

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
img :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:224
img = Open "<img" ">"                                                           -- util/GenerateHtmlVariant.hs:225
{-# INLINE img #-}                                                              -- util/GenerateHtmlVariant.hs:226

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
input :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:224
input = Open "<input" ">"                                                       -- util/GenerateHtmlVariant.hs:225
{-# INLINE input #-}                                                            -- util/GenerateHtmlVariant.hs:226

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
ins :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
ins = Parent "<ins" "</ins>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE ins #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
kbd :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
kbd = Parent "<kbd" "</kbd>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE kbd #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
label :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
label = Parent "<label" "</label>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE label #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
legend :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
legend = Parent "<legend" "</legend>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE legend #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
li :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
li = Parent "<li" "</li>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE li #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
link :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:224
link = Open "<link" ">"                                                         -- util/GenerateHtmlVariant.hs:225
{-# INLINE link #-}                                                             -- util/GenerateHtmlVariant.hs:226

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
map :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
map = Parent "<map" "</map>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE map #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
meta :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:224
meta = Open "<meta" ">"                                                         -- util/GenerateHtmlVariant.hs:225
{-# INLINE meta #-}                                                             -- util/GenerateHtmlVariant.hs:226

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
noscript :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:181
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:182
noscript = Parent "<noscript" "</noscript>"                                     -- util/GenerateHtmlVariant.hs:183
{-# INLINE noscript #-}                                                         -- util/GenerateHtmlVariant.hs:184

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
object :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
object = Parent "<object" "</object>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE object #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
ol :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
ol = Parent "<ol" "</ol>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE ol #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
optgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:181
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:182
optgroup = Parent "<optgroup" "</optgroup>"                                     -- util/GenerateHtmlVariant.hs:183
{-# INLINE optgroup #-}                                                         -- util/GenerateHtmlVariant.hs:184

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
option :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
option = Parent "<option" "</option>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE option #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
p :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:181
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:182
p = Parent "<p" "</p>"                                                          -- util/GenerateHtmlVariant.hs:183
{-# INLINE p #-}                                                                -- util/GenerateHtmlVariant.hs:184

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
param :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:224
param = Open "<param" ">"                                                       -- util/GenerateHtmlVariant.hs:225
{-# INLINE param #-}                                                            -- util/GenerateHtmlVariant.hs:226

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
pre :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
pre = Parent "<pre" "</pre>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE pre #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
q :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:181
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:182
q = Parent "<q" "</q>"                                                          -- util/GenerateHtmlVariant.hs:183
{-# INLINE q #-}                                                                -- util/GenerateHtmlVariant.hs:184

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
samp :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
samp = Parent "<samp" "</samp>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE samp #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
script :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
script = Parent "<script" "</script>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE script #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
select :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
select = Parent "<select" "</select>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE select #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
small :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
small = Parent "<small" "</small>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE small #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
span :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:181
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:182
span = Parent "<span" "</span>"                                                 -- util/GenerateHtmlVariant.hs:183
{-# INLINE span #-}                                                             -- util/GenerateHtmlVariant.hs:184

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
strong :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:181
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:182
strong = Parent "<strong" "</strong>"                                           -- util/GenerateHtmlVariant.hs:183
{-# INLINE strong #-}                                                           -- util/GenerateHtmlVariant.hs:184

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
style :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
style = Parent "<style" "</style>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE style #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
sub :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
sub = Parent "<sub" "</sub>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE sub #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
sup :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
sup = Parent "<sup" "</sup>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE sup #-}                                                              -- util/GenerateHtmlVariant.hs:184

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
table :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
table = Parent "<table" "</table>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE table #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
tbody :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
tbody = Parent "<tbody" "</tbody>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE tbody #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
td :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
td = Parent "<td" "</td>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE td #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
textarea :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:181
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:182
textarea = Parent "<textarea" "</textarea>"                                     -- util/GenerateHtmlVariant.hs:183
{-# INLINE textarea #-}                                                         -- util/GenerateHtmlVariant.hs:184

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
tfoot :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
tfoot = Parent "<tfoot" "</tfoot>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE tfoot #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
th :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
th = Parent "<th" "</th>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE th #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
thead :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
thead = Parent "<thead" "</thead>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE thead #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
title :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:181
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:182
title = Parent "<title" "</title>"                                              -- util/GenerateHtmlVariant.hs:183
{-# INLINE title #-}                                                            -- util/GenerateHtmlVariant.hs:184

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
tr :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
tr = Parent "<tr" "</tr>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE tr #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
tt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
tt = Parent "<tt" "</tt>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE tt #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
ul :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:181
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:182
ul = Parent "<ul" "</ul>"                                                       -- util/GenerateHtmlVariant.hs:183
{-# INLINE ul #-}                                                               -- util/GenerateHtmlVariant.hs:184

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
var :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:181
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:182
var = Parent "<var" "</var>"                                                    -- util/GenerateHtmlVariant.hs:183
{-# INLINE var #-}                                                              -- util/GenerateHtmlVariant.hs:184
