-- WARNING: This code was automatically generated. You should *never*
-- edit it directly. Instead, edit the files who generated this code,
-- you can find them in the @util/@ directory.

{-# LANGUAGE OverloadedStrings #-}                                              -- util/GenerateHtmlVariant.hs:49
-- | This module exports HTML combinators used to create documents.
--
module Text.Blaze.Html5                                                         -- util/GenerateHtmlVariant.hs:123
    ( module Text.Blaze                                                         -- util/GenerateHtmlVariant.hs:124
    , html                                                                      -- util/GenerateHtmlVariant.hs:126
    , docType                                                                   -- util/GenerateHtmlVariant.hs:126
    , a                                                                         -- util/GenerateHtmlVariant.hs:126
    , abbr                                                                      -- util/GenerateHtmlVariant.hs:126
    , address                                                                   -- util/GenerateHtmlVariant.hs:126
    , area                                                                      -- util/GenerateHtmlVariant.hs:126
    , article                                                                   -- util/GenerateHtmlVariant.hs:126
    , aside                                                                     -- util/GenerateHtmlVariant.hs:126
    , audio                                                                     -- util/GenerateHtmlVariant.hs:126
    , b                                                                         -- util/GenerateHtmlVariant.hs:126
    , base                                                                      -- util/GenerateHtmlVariant.hs:126
    , bdo                                                                       -- util/GenerateHtmlVariant.hs:126
    , blockquote                                                                -- util/GenerateHtmlVariant.hs:126
    , body                                                                      -- util/GenerateHtmlVariant.hs:126
    , br                                                                        -- util/GenerateHtmlVariant.hs:126
    , button                                                                    -- util/GenerateHtmlVariant.hs:126
    , canvas                                                                    -- util/GenerateHtmlVariant.hs:126
    , caption                                                                   -- util/GenerateHtmlVariant.hs:126
    , cite                                                                      -- util/GenerateHtmlVariant.hs:126
    , code                                                                      -- util/GenerateHtmlVariant.hs:126
    , col                                                                       -- util/GenerateHtmlVariant.hs:126
    , colgroup                                                                  -- util/GenerateHtmlVariant.hs:126
    , command                                                                   -- util/GenerateHtmlVariant.hs:126
    , datalist                                                                  -- util/GenerateHtmlVariant.hs:126
    , dd                                                                        -- util/GenerateHtmlVariant.hs:126
    , del                                                                       -- util/GenerateHtmlVariant.hs:126
    , details                                                                   -- util/GenerateHtmlVariant.hs:126
    , dfn                                                                       -- util/GenerateHtmlVariant.hs:126
    , div                                                                       -- util/GenerateHtmlVariant.hs:126
    , dl                                                                        -- util/GenerateHtmlVariant.hs:126
    , dt                                                                        -- util/GenerateHtmlVariant.hs:126
    , em                                                                        -- util/GenerateHtmlVariant.hs:126
    , embed                                                                     -- util/GenerateHtmlVariant.hs:126
    , fieldset                                                                  -- util/GenerateHtmlVariant.hs:126
    , figcaption                                                                -- util/GenerateHtmlVariant.hs:126
    , figure                                                                    -- util/GenerateHtmlVariant.hs:126
    , footer                                                                    -- util/GenerateHtmlVariant.hs:126
    , form                                                                      -- util/GenerateHtmlVariant.hs:126
    , h1                                                                        -- util/GenerateHtmlVariant.hs:126
    , h2                                                                        -- util/GenerateHtmlVariant.hs:126
    , h3                                                                        -- util/GenerateHtmlVariant.hs:126
    , h4                                                                        -- util/GenerateHtmlVariant.hs:126
    , h5                                                                        -- util/GenerateHtmlVariant.hs:126
    , h6                                                                        -- util/GenerateHtmlVariant.hs:126
    , head                                                                      -- util/GenerateHtmlVariant.hs:126
    , header                                                                    -- util/GenerateHtmlVariant.hs:126
    , hgroup                                                                    -- util/GenerateHtmlVariant.hs:126
    , hr                                                                        -- util/GenerateHtmlVariant.hs:126
    , htmlNoDocType                                                             -- util/GenerateHtmlVariant.hs:126
    , i                                                                         -- util/GenerateHtmlVariant.hs:126
    , iframe                                                                    -- util/GenerateHtmlVariant.hs:126
    , img                                                                       -- util/GenerateHtmlVariant.hs:126
    , input                                                                     -- util/GenerateHtmlVariant.hs:126
    , ins                                                                       -- util/GenerateHtmlVariant.hs:126
    , kbd                                                                       -- util/GenerateHtmlVariant.hs:126
    , keygen                                                                    -- util/GenerateHtmlVariant.hs:126
    , label                                                                     -- util/GenerateHtmlVariant.hs:126
    , legend                                                                    -- util/GenerateHtmlVariant.hs:126
    , li                                                                        -- util/GenerateHtmlVariant.hs:126
    , link                                                                      -- util/GenerateHtmlVariant.hs:126
    , map                                                                       -- util/GenerateHtmlVariant.hs:126
    , mark                                                                      -- util/GenerateHtmlVariant.hs:126
    , menu                                                                      -- util/GenerateHtmlVariant.hs:126
    , meta                                                                      -- util/GenerateHtmlVariant.hs:126
    , meter                                                                     -- util/GenerateHtmlVariant.hs:126
    , nav                                                                       -- util/GenerateHtmlVariant.hs:126
    , noscript                                                                  -- util/GenerateHtmlVariant.hs:126
    , object                                                                    -- util/GenerateHtmlVariant.hs:126
    , ol                                                                        -- util/GenerateHtmlVariant.hs:126
    , optgroup                                                                  -- util/GenerateHtmlVariant.hs:126
    , option                                                                    -- util/GenerateHtmlVariant.hs:126
    , output                                                                    -- util/GenerateHtmlVariant.hs:126
    , p                                                                         -- util/GenerateHtmlVariant.hs:126
    , param                                                                     -- util/GenerateHtmlVariant.hs:126
    , pre                                                                       -- util/GenerateHtmlVariant.hs:126
    , progress                                                                  -- util/GenerateHtmlVariant.hs:126
    , q                                                                         -- util/GenerateHtmlVariant.hs:126
    , rp                                                                        -- util/GenerateHtmlVariant.hs:126
    , rt                                                                        -- util/GenerateHtmlVariant.hs:126
    , ruby                                                                      -- util/GenerateHtmlVariant.hs:126
    , samp                                                                      -- util/GenerateHtmlVariant.hs:126
    , script                                                                    -- util/GenerateHtmlVariant.hs:126
    , section                                                                   -- util/GenerateHtmlVariant.hs:126
    , select                                                                    -- util/GenerateHtmlVariant.hs:126
    , small                                                                     -- util/GenerateHtmlVariant.hs:126
    , source                                                                    -- util/GenerateHtmlVariant.hs:126
    , span                                                                      -- util/GenerateHtmlVariant.hs:126
    , strong                                                                    -- util/GenerateHtmlVariant.hs:126
    , style                                                                     -- util/GenerateHtmlVariant.hs:126
    , sub                                                                       -- util/GenerateHtmlVariant.hs:126
    , summary                                                                   -- util/GenerateHtmlVariant.hs:126
    , sup                                                                       -- util/GenerateHtmlVariant.hs:126
    , table                                                                     -- util/GenerateHtmlVariant.hs:126
    , tbody                                                                     -- util/GenerateHtmlVariant.hs:126
    , td                                                                        -- util/GenerateHtmlVariant.hs:126
    , textarea                                                                  -- util/GenerateHtmlVariant.hs:126
    , tfoot                                                                     -- util/GenerateHtmlVariant.hs:126
    , th                                                                        -- util/GenerateHtmlVariant.hs:126
    , thead                                                                     -- util/GenerateHtmlVariant.hs:126
    , time                                                                      -- util/GenerateHtmlVariant.hs:126
    , title                                                                     -- util/GenerateHtmlVariant.hs:126
    , tr                                                                        -- util/GenerateHtmlVariant.hs:126
    , ul                                                                        -- util/GenerateHtmlVariant.hs:126
    , var                                                                       -- util/GenerateHtmlVariant.hs:126
    , video                                                                     -- util/GenerateHtmlVariant.hs:126
    ) where                                                                     -- util/GenerateHtmlVariant.hs:127

import Prelude ((>>))                                                           -- util/GenerateHtmlVariant.hs:56
import Data.Monoid (mappend)                                                    -- util/GenerateHtmlVariant.hs:57
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
-- > <!DOCTYPE HTML>
-- > <html><span>foo</span></html>
--
html :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:164
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:165
html inner = docType >> htmlNoDocType inner                                     -- util/GenerateHtmlVariant.hs:166
{-# INLINE html #-}                                                             -- util/GenerateHtmlVariant.hs:167

-- | Combinator for the document type. This should be placed at the top
-- of every HTML page.
--
-- Example:
--
-- > docType
--
-- Result:
--
-- > <!DOCTYPE HTML>
--
docType :: Html  -- ^ The document type HTML.                                   -- util/GenerateHtmlVariant.hs:143
docType = preEscapedText "<!DOCTYPE HTML>\n"                                    -- util/GenerateHtmlVariant.hs:144
{-# INLINE docType #-}                                                          -- util/GenerateHtmlVariant.hs:145

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
a :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:184
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:185
a = Parent "<a" "</a>"                                                          -- util/GenerateHtmlVariant.hs:186
{-# INLINE a #-}                                                                -- util/GenerateHtmlVariant.hs:187

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
abbr :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
abbr = Parent "<abbr" "</abbr>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE abbr #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
address :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
address = Parent "<address" "</address>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE address #-}                                                          -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<area />@ element.
--
-- Example:
--
-- > area
--
-- Result:
--
-- > <area />
--
area :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:206
area = Leaf "<area" " />"                                                       -- util/GenerateHtmlVariant.hs:207
{-# INLINE area #-}                                                             -- util/GenerateHtmlVariant.hs:208

-- | Combinator for the @\<article>@ element.
--
-- Example:
--
-- > article $ span $ text "foo"
--
-- Result:
--
-- > <article><span>foo</span></article>
--
article :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
article = Parent "<article" "</article>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE article #-}                                                          -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<aside>@ element.
--
-- Example:
--
-- > aside $ span $ text "foo"
--
-- Result:
--
-- > <aside><span>foo</span></aside>
--
aside :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
aside = Parent "<aside" "</aside>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE aside #-}                                                            -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<audio>@ element.
--
-- Example:
--
-- > audio $ span $ text "foo"
--
-- Result:
--
-- > <audio><span>foo</span></audio>
--
audio :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
audio = Parent "<audio" "</audio>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE audio #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
b :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:184
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:185
b = Parent "<b" "</b>"                                                          -- util/GenerateHtmlVariant.hs:186
{-# INLINE b #-}                                                                -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<base>@ element.
--
-- Example:
--
-- > base $ span $ text "foo"
--
-- Result:
--
-- > <base><span>foo</span></base>
--
base :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
base = Parent "<base" "</base>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE base #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
bdo :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
bdo = Parent "<bdo" "</bdo>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE bdo #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
blockquote :: Html  -- ^ Inner HTML.                                            -- util/GenerateHtmlVariant.hs:184
           -> Html  -- ^ Resulting HTML.                                        -- util/GenerateHtmlVariant.hs:185
blockquote = Parent "<blockquote" "</blockquote>"                               -- util/GenerateHtmlVariant.hs:186
{-# INLINE blockquote #-}                                                       -- util/GenerateHtmlVariant.hs:187

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
body :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
body = Parent "<body" "</body>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE body #-}                                                             -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<br />@ element.
--
-- Example:
--
-- > br
--
-- Result:
--
-- > <br />
--
br :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:206
br = Leaf "<br" " />"                                                           -- util/GenerateHtmlVariant.hs:207
{-# INLINE br #-}                                                               -- util/GenerateHtmlVariant.hs:208

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
button :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
button = Parent "<button" "</button>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE button #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<canvas>@ element.
--
-- Example:
--
-- > canvas $ span $ text "foo"
--
-- Result:
--
-- > <canvas><span>foo</span></canvas>
--
canvas :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
canvas = Parent "<canvas" "</canvas>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE canvas #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
caption :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
caption = Parent "<caption" "</caption>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE caption #-}                                                          -- util/GenerateHtmlVariant.hs:187

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
cite :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
cite = Parent "<cite" "</cite>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE cite #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
code :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
code = Parent "<code" "</code>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE code #-}                                                             -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<col />@ element.
--
-- Example:
--
-- > col
--
-- Result:
--
-- > <col />
--
col :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:206
col = Leaf "<col" " />"                                                         -- util/GenerateHtmlVariant.hs:207
{-# INLINE col #-}                                                              -- util/GenerateHtmlVariant.hs:208

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
colgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
colgroup = Parent "<colgroup" "</colgroup>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE colgroup #-}                                                         -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<command>@ element.
--
-- Example:
--
-- > command $ span $ text "foo"
--
-- Result:
--
-- > <command><span>foo</span></command>
--
command :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
command = Parent "<command" "</command>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE command #-}                                                          -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<datalist>@ element.
--
-- Example:
--
-- > datalist $ span $ text "foo"
--
-- Result:
--
-- > <datalist><span>foo</span></datalist>
--
datalist :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
datalist = Parent "<datalist" "</datalist>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE datalist #-}                                                         -- util/GenerateHtmlVariant.hs:187

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
dd :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
dd = Parent "<dd" "</dd>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE dd #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
del :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
del = Parent "<del" "</del>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE del #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<details>@ element.
--
-- Example:
--
-- > details $ span $ text "foo"
--
-- Result:
--
-- > <details><span>foo</span></details>
--
details :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
details = Parent "<details" "</details>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE details #-}                                                          -- util/GenerateHtmlVariant.hs:187

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
dfn :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
dfn = Parent "<dfn" "</dfn>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE dfn #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
div :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
div = Parent "<div" "</div>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE div #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
dl :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
dl = Parent "<dl" "</dl>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE dl #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
dt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
dt = Parent "<dt" "</dt>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE dt #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
em :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
em = Parent "<em" "</em>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE em #-}                                                               -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<embed />@ element.
--
-- Example:
--
-- > embed
--
-- Result:
--
-- > <embed />
--
embed :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:206
embed = Leaf "<embed" " />"                                                     -- util/GenerateHtmlVariant.hs:207
{-# INLINE embed #-}                                                            -- util/GenerateHtmlVariant.hs:208

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
fieldset :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
fieldset = Parent "<fieldset" "</fieldset>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE fieldset #-}                                                         -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<figcaption>@ element.
--
-- Example:
--
-- > figcaption $ span $ text "foo"
--
-- Result:
--
-- > <figcaption><span>foo</span></figcaption>
--
figcaption :: Html  -- ^ Inner HTML.                                            -- util/GenerateHtmlVariant.hs:184
           -> Html  -- ^ Resulting HTML.                                        -- util/GenerateHtmlVariant.hs:185
figcaption = Parent "<figcaption" "</figcaption>"                               -- util/GenerateHtmlVariant.hs:186
{-# INLINE figcaption #-}                                                       -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<figure>@ element.
--
-- Example:
--
-- > figure $ span $ text "foo"
--
-- Result:
--
-- > <figure><span>foo</span></figure>
--
figure :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
figure = Parent "<figure" "</figure>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE figure #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<footer>@ element.
--
-- Example:
--
-- > footer $ span $ text "foo"
--
-- Result:
--
-- > <footer><span>foo</span></footer>
--
footer :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
footer = Parent "<footer" "</footer>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE footer #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
form :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
form = Parent "<form" "</form>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE form #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
h1 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h1 = Parent "<h1" "</h1>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h1 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
h2 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h2 = Parent "<h2" "</h2>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h2 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
h3 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h3 = Parent "<h3" "</h3>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h3 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
h4 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h4 = Parent "<h4" "</h4>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h4 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
h5 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h5 = Parent "<h5" "</h5>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h5 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
h6 :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
h6 = Parent "<h6" "</h6>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE h6 #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
head :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
head = Parent "<head" "</head>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE head #-}                                                             -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<header>@ element.
--
-- Example:
--
-- > header $ span $ text "foo"
--
-- Result:
--
-- > <header><span>foo</span></header>
--
header :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
header = Parent "<header" "</header>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE header #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<hgroup>@ element.
--
-- Example:
--
-- > hgroup $ span $ text "foo"
--
-- Result:
--
-- > <hgroup><span>foo</span></hgroup>
--
hgroup :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
hgroup = Parent "<hgroup" "</hgroup>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE hgroup #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<hr />@ element.
--
-- Example:
--
-- > hr
--
-- Result:
--
-- > <hr />
--
hr :: Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:206
hr = Leaf "<hr" " />"                                                           -- util/GenerateHtmlVariant.hs:207
{-# INLINE hr #-}                                                               -- util/GenerateHtmlVariant.hs:208

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
htmlNoDocType :: Html  -- ^ Inner HTML.                                         -- util/GenerateHtmlVariant.hs:184
              -> Html  -- ^ Resulting HTML.                                     -- util/GenerateHtmlVariant.hs:185
htmlNoDocType = Parent "<html" "</html>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE htmlNoDocType #-}                                                    -- util/GenerateHtmlVariant.hs:187

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
i :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:184
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:185
i = Parent "<i" "</i>"                                                          -- util/GenerateHtmlVariant.hs:186
{-# INLINE i #-}                                                                -- util/GenerateHtmlVariant.hs:187

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
iframe :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
iframe = Parent "<iframe" "</iframe>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE iframe #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<img />@ element.
--
-- Example:
--
-- > img
--
-- Result:
--
-- > <img />
--
img :: Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:206
img = Leaf "<img" " />"                                                         -- util/GenerateHtmlVariant.hs:207
{-# INLINE img #-}                                                              -- util/GenerateHtmlVariant.hs:208

-- | Combinator for the @\<input />@ element.
--
-- Example:
--
-- > input
--
-- Result:
--
-- > <input />
--
input :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:206
input = Leaf "<input" " />"                                                     -- util/GenerateHtmlVariant.hs:207
{-# INLINE input #-}                                                            -- util/GenerateHtmlVariant.hs:208

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
ins :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
ins = Parent "<ins" "</ins>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE ins #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
kbd :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
kbd = Parent "<kbd" "</kbd>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE kbd #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<keygen>@ element.
--
-- Example:
--
-- > keygen $ span $ text "foo"
--
-- Result:
--
-- > <keygen><span>foo</span></keygen>
--
keygen :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
keygen = Parent "<keygen" "</keygen>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE keygen #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
label :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
label = Parent "<label" "</label>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE label #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
legend :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
legend = Parent "<legend" "</legend>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE legend #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
li :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
li = Parent "<li" "</li>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE li #-}                                                               -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<link />@ element.
--
-- Example:
--
-- > link
--
-- Result:
--
-- > <link />
--
link :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:206
link = Leaf "<link" " />"                                                       -- util/GenerateHtmlVariant.hs:207
{-# INLINE link #-}                                                             -- util/GenerateHtmlVariant.hs:208

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
map :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
map = Parent "<map" "</map>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE map #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<mark>@ element.
--
-- Example:
--
-- > mark $ span $ text "foo"
--
-- Result:
--
-- > <mark><span>foo</span></mark>
--
mark :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
mark = Parent "<mark" "</mark>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE mark #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
menu :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
menu = Parent "<menu" "</menu>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE menu #-}                                                             -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<meta />@ element.
--
-- Example:
--
-- > meta
--
-- Result:
--
-- > <meta />
--
meta :: Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:206
meta = Leaf "<meta" " />"                                                       -- util/GenerateHtmlVariant.hs:207
{-# INLINE meta #-}                                                             -- util/GenerateHtmlVariant.hs:208

-- | Combinator for the @\<meter>@ element.
--
-- Example:
--
-- > meter $ span $ text "foo"
--
-- Result:
--
-- > <meter><span>foo</span></meter>
--
meter :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
meter = Parent "<meter" "</meter>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE meter #-}                                                            -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<nav>@ element.
--
-- Example:
--
-- > nav $ span $ text "foo"
--
-- Result:
--
-- > <nav><span>foo</span></nav>
--
nav :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
nav = Parent "<nav" "</nav>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE nav #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
noscript :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
noscript = Parent "<noscript" "</noscript>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE noscript #-}                                                         -- util/GenerateHtmlVariant.hs:187

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
object :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
object = Parent "<object" "</object>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE object #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
ol :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
ol = Parent "<ol" "</ol>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE ol #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
optgroup :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
optgroup = Parent "<optgroup" "</optgroup>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE optgroup #-}                                                         -- util/GenerateHtmlVariant.hs:187

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
option :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
option = Parent "<option" "</option>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE option #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<output>@ element.
--
-- Example:
--
-- > output $ span $ text "foo"
--
-- Result:
--
-- > <output><span>foo</span></output>
--
output :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
output = Parent "<output" "</output>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE output #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
p :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:184
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:185
p = Parent "<p" "</p>"                                                          -- util/GenerateHtmlVariant.hs:186
{-# INLINE p #-}                                                                -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<param />@ element.
--
-- Example:
--
-- > param
--
-- Result:
--
-- > <param />
--
param :: Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:206
param = Leaf "<param" " />"                                                     -- util/GenerateHtmlVariant.hs:207
{-# INLINE param #-}                                                            -- util/GenerateHtmlVariant.hs:208

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
pre :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
pre = Parent "<pre" "</pre>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE pre #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<progress>@ element.
--
-- Example:
--
-- > progress $ span $ text "foo"
--
-- Result:
--
-- > <progress><span>foo</span></progress>
--
progress :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
progress = Parent "<progress" "</progress>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE progress #-}                                                         -- util/GenerateHtmlVariant.hs:187

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
q :: Html  -- ^ Inner HTML.                                                     -- util/GenerateHtmlVariant.hs:184
  -> Html  -- ^ Resulting HTML.                                                 -- util/GenerateHtmlVariant.hs:185
q = Parent "<q" "</q>"                                                          -- util/GenerateHtmlVariant.hs:186
{-# INLINE q #-}                                                                -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<rp>@ element.
--
-- Example:
--
-- > rp $ span $ text "foo"
--
-- Result:
--
-- > <rp><span>foo</span></rp>
--
rp :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
rp = Parent "<rp" "</rp>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE rp #-}                                                               -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<rt>@ element.
--
-- Example:
--
-- > rt $ span $ text "foo"
--
-- Result:
--
-- > <rt><span>foo</span></rt>
--
rt :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
rt = Parent "<rt" "</rt>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE rt #-}                                                               -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<ruby>@ element.
--
-- Example:
--
-- > ruby $ span $ text "foo"
--
-- Result:
--
-- > <ruby><span>foo</span></ruby>
--
ruby :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
ruby = Parent "<ruby" "</ruby>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE ruby #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
samp :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
samp = Parent "<samp" "</samp>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE samp #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
script :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
script = Parent "<script" "</script>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE script #-}                                                           -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<section>@ element.
--
-- Example:
--
-- > section $ span $ text "foo"
--
-- Result:
--
-- > <section><span>foo</span></section>
--
section :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
section = Parent "<section" "</section>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE section #-}                                                          -- util/GenerateHtmlVariant.hs:187

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
select :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
select = Parent "<select" "</select>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE select #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
small :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
small = Parent "<small" "</small>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE small #-}                                                            -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<source>@ element.
--
-- Example:
--
-- > source $ span $ text "foo"
--
-- Result:
--
-- > <source><span>foo</span></source>
--
source :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
source = Parent "<source" "</source>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE source #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
span :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
span = Parent "<span" "</span>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE span #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
strong :: Html  -- ^ Inner HTML.                                                -- util/GenerateHtmlVariant.hs:184
       -> Html  -- ^ Resulting HTML.                                            -- util/GenerateHtmlVariant.hs:185
strong = Parent "<strong" "</strong>"                                           -- util/GenerateHtmlVariant.hs:186
{-# INLINE strong #-}                                                           -- util/GenerateHtmlVariant.hs:187

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
style :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
style = Parent "<style" "</style>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE style #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
sub :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
sub = Parent "<sub" "</sub>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE sub #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<summary>@ element.
--
-- Example:
--
-- > summary $ span $ text "foo"
--
-- Result:
--
-- > <summary><span>foo</span></summary>
--
summary :: Html  -- ^ Inner HTML.                                               -- util/GenerateHtmlVariant.hs:184
        -> Html  -- ^ Resulting HTML.                                           -- util/GenerateHtmlVariant.hs:185
summary = Parent "<summary" "</summary>"                                        -- util/GenerateHtmlVariant.hs:186
{-# INLINE summary #-}                                                          -- util/GenerateHtmlVariant.hs:187

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
sup :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
sup = Parent "<sup" "</sup>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE sup #-}                                                              -- util/GenerateHtmlVariant.hs:187

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
table :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
table = Parent "<table" "</table>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE table #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
tbody :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
tbody = Parent "<tbody" "</tbody>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE tbody #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
td :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
td = Parent "<td" "</td>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE td #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
textarea :: Html  -- ^ Inner HTML.                                              -- util/GenerateHtmlVariant.hs:184
         -> Html  -- ^ Resulting HTML.                                          -- util/GenerateHtmlVariant.hs:185
textarea = Parent "<textarea" "</textarea>"                                     -- util/GenerateHtmlVariant.hs:186
{-# INLINE textarea #-}                                                         -- util/GenerateHtmlVariant.hs:187

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
tfoot :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
tfoot = Parent "<tfoot" "</tfoot>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE tfoot #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
th :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
th = Parent "<th" "</th>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE th #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
thead :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
thead = Parent "<thead" "</thead>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE thead #-}                                                            -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<time>@ element.
--
-- Example:
--
-- > time $ span $ text "foo"
--
-- Result:
--
-- > <time><span>foo</span></time>
--
time :: Html  -- ^ Inner HTML.                                                  -- util/GenerateHtmlVariant.hs:184
     -> Html  -- ^ Resulting HTML.                                              -- util/GenerateHtmlVariant.hs:185
time = Parent "<time" "</time>"                                                 -- util/GenerateHtmlVariant.hs:186
{-# INLINE time #-}                                                             -- util/GenerateHtmlVariant.hs:187

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
title :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
title = Parent "<title" "</title>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE title #-}                                                            -- util/GenerateHtmlVariant.hs:187

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
tr :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
tr = Parent "<tr" "</tr>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE tr #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
ul :: Html  -- ^ Inner HTML.                                                    -- util/GenerateHtmlVariant.hs:184
   -> Html  -- ^ Resulting HTML.                                                -- util/GenerateHtmlVariant.hs:185
ul = Parent "<ul" "</ul>"                                                       -- util/GenerateHtmlVariant.hs:186
{-# INLINE ul #-}                                                               -- util/GenerateHtmlVariant.hs:187

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
var :: Html  -- ^ Inner HTML.                                                   -- util/GenerateHtmlVariant.hs:184
    -> Html  -- ^ Resulting HTML.                                               -- util/GenerateHtmlVariant.hs:185
var = Parent "<var" "</var>"                                                    -- util/GenerateHtmlVariant.hs:186
{-# INLINE var #-}                                                              -- util/GenerateHtmlVariant.hs:187

-- | Combinator for the @\<video>@ element.
--
-- Example:
--
-- > video $ span $ text "foo"
--
-- Result:
--
-- > <video><span>foo</span></video>
--
video :: Html  -- ^ Inner HTML.                                                 -- util/GenerateHtmlVariant.hs:184
      -> Html  -- ^ Resulting HTML.                                             -- util/GenerateHtmlVariant.hs:185
video = Parent "<video" "</video>"                                              -- util/GenerateHtmlVariant.hs:186
{-# INLINE video #-}                                                            -- util/GenerateHtmlVariant.hs:187
