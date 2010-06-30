{-# LANGUAGE OverloadedStrings #-}                                              -- GenerateHtmlVariant.hs:48
-- | This module exports HTML combinators used to create documents.
--
module Text.Blaze.Html4.Strict                                                  -- GenerateHtmlVariant.hs:111
    ( module Text.Blaze                                                         -- GenerateHtmlVariant.hs:112
    , html                                                                      -- GenerateHtmlVariant.hs:114
    , docType                                                                   -- GenerateHtmlVariant.hs:114
    , a                                                                         -- GenerateHtmlVariant.hs:114
    , abbr                                                                      -- GenerateHtmlVariant.hs:114
    , acronym                                                                   -- GenerateHtmlVariant.hs:114
    , address                                                                   -- GenerateHtmlVariant.hs:114
    , area                                                                      -- GenerateHtmlVariant.hs:114
    , b                                                                         -- GenerateHtmlVariant.hs:114
    , bdo                                                                       -- GenerateHtmlVariant.hs:114
    , big                                                                       -- GenerateHtmlVariant.hs:114
    , blockquote                                                                -- GenerateHtmlVariant.hs:114
    , body                                                                      -- GenerateHtmlVariant.hs:114
    , br                                                                        -- GenerateHtmlVariant.hs:114
    , button                                                                    -- GenerateHtmlVariant.hs:114
    , caption                                                                   -- GenerateHtmlVariant.hs:114
    , cite                                                                      -- GenerateHtmlVariant.hs:114
    , code                                                                      -- GenerateHtmlVariant.hs:114
    , col                                                                       -- GenerateHtmlVariant.hs:114
    , colgroup                                                                  -- GenerateHtmlVariant.hs:114
    , dd                                                                        -- GenerateHtmlVariant.hs:114
    , del                                                                       -- GenerateHtmlVariant.hs:114
    , dfn                                                                       -- GenerateHtmlVariant.hs:114
    , div                                                                       -- GenerateHtmlVariant.hs:114
    , dl                                                                        -- GenerateHtmlVariant.hs:114
    , dt                                                                        -- GenerateHtmlVariant.hs:114
    , em                                                                        -- GenerateHtmlVariant.hs:114
    , fieldset                                                                  -- GenerateHtmlVariant.hs:114
    , form                                                                      -- GenerateHtmlVariant.hs:114
    , h1                                                                        -- GenerateHtmlVariant.hs:114
    , h2                                                                        -- GenerateHtmlVariant.hs:114
    , h3                                                                        -- GenerateHtmlVariant.hs:114
    , h4                                                                        -- GenerateHtmlVariant.hs:114
    , h5                                                                        -- GenerateHtmlVariant.hs:114
    , h6                                                                        -- GenerateHtmlVariant.hs:114
    , head                                                                      -- GenerateHtmlVariant.hs:114
    , hr                                                                        -- GenerateHtmlVariant.hs:114
    , htmlNoDocType                                                             -- GenerateHtmlVariant.hs:114
    , i                                                                         -- GenerateHtmlVariant.hs:114
    , img                                                                       -- GenerateHtmlVariant.hs:114
    , input                                                                     -- GenerateHtmlVariant.hs:114
    , ins                                                                       -- GenerateHtmlVariant.hs:114
    , kbd                                                                       -- GenerateHtmlVariant.hs:114
    , label                                                                     -- GenerateHtmlVariant.hs:114
    , legend                                                                    -- GenerateHtmlVariant.hs:114
    , li                                                                        -- GenerateHtmlVariant.hs:114
    , link                                                                      -- GenerateHtmlVariant.hs:114
    , map                                                                       -- GenerateHtmlVariant.hs:114
    , meta                                                                      -- GenerateHtmlVariant.hs:114
    , noscript                                                                  -- GenerateHtmlVariant.hs:114
    , object                                                                    -- GenerateHtmlVariant.hs:114
    , ol                                                                        -- GenerateHtmlVariant.hs:114
    , optgroup                                                                  -- GenerateHtmlVariant.hs:114
    , option                                                                    -- GenerateHtmlVariant.hs:114
    , p                                                                         -- GenerateHtmlVariant.hs:114
    , param                                                                     -- GenerateHtmlVariant.hs:114
    , pre                                                                       -- GenerateHtmlVariant.hs:114
    , q                                                                         -- GenerateHtmlVariant.hs:114
    , samp                                                                      -- GenerateHtmlVariant.hs:114
    , script                                                                    -- GenerateHtmlVariant.hs:114
    , select                                                                    -- GenerateHtmlVariant.hs:114
    , small                                                                     -- GenerateHtmlVariant.hs:114
    , span                                                                      -- GenerateHtmlVariant.hs:114
    , strong                                                                    -- GenerateHtmlVariant.hs:114
    , style                                                                     -- GenerateHtmlVariant.hs:114
    , sub                                                                       -- GenerateHtmlVariant.hs:114
    , sup                                                                       -- GenerateHtmlVariant.hs:114
    , table                                                                     -- GenerateHtmlVariant.hs:114
    , tbody                                                                     -- GenerateHtmlVariant.hs:114
    , td                                                                        -- GenerateHtmlVariant.hs:114
    , textarea                                                                  -- GenerateHtmlVariant.hs:114
    , tfoot                                                                     -- GenerateHtmlVariant.hs:114
    , th                                                                        -- GenerateHtmlVariant.hs:114
    , thead                                                                     -- GenerateHtmlVariant.hs:114
    , title                                                                     -- GenerateHtmlVariant.hs:114
    , tr                                                                        -- GenerateHtmlVariant.hs:114
    , tt                                                                        -- GenerateHtmlVariant.hs:114
    , ul                                                                        -- GenerateHtmlVariant.hs:114
    , var                                                                       -- GenerateHtmlVariant.hs:114
    ) where                                                                     -- GenerateHtmlVariant.hs:115

import Prelude ((>>))                                                           -- GenerateHtmlVariant.hs:55
                                                                                -- GenerateHtmlVariant.hs:56
import Text.Blaze                                                               -- GenerateHtmlVariant.hs:57
import Text.Blaze.Internal (parent, leaf, open)                                 -- GenerateHtmlVariant.hs:58
                                                                                -- GenerateHtmlVariant.hs:59
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
html :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:152
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:153
html inner = docType >> htmlNoDocType inner                                     -- GenerateHtmlVariant.hs:154
{-# INLINE html #-}                                                             -- GenerateHtmlVariant.hs:155

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
docType :: HtmlA   -- ^ The document type HTML.                                 -- GenerateHtmlVariant.hs:131
docType = preEscapedText "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n    \"http://www.w3.org/TR/html4/strict.dtd\">\n" -- GenerateHtmlVariant.hs:132
{-# INLINE docType #-}                                                          -- GenerateHtmlVariant.hs:133

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
a :: HtmlA   -- ^ Inner HTML.                                                   -- GenerateHtmlVariant.hs:172
  -> HtmlA   -- ^ Resulting HTML.                                               -- GenerateHtmlVariant.hs:173
a = parent "<a" "</a>"                                                          -- GenerateHtmlVariant.hs:174
{-# INLINE a #-}                                                                -- GenerateHtmlVariant.hs:175

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
abbr :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
abbr = parent "<abbr" "</abbr>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE abbr #-}                                                             -- GenerateHtmlVariant.hs:175

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
acronym :: HtmlA   -- ^ Inner HTML.                                             -- GenerateHtmlVariant.hs:172
        -> HtmlA   -- ^ Resulting HTML.                                         -- GenerateHtmlVariant.hs:173
acronym = parent "<acronym" "</acronym>"                                        -- GenerateHtmlVariant.hs:174
{-# INLINE acronym #-}                                                          -- GenerateHtmlVariant.hs:175

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
address :: HtmlA   -- ^ Inner HTML.                                             -- GenerateHtmlVariant.hs:172
        -> HtmlA   -- ^ Resulting HTML.                                         -- GenerateHtmlVariant.hs:173
address = parent "<address" "</address>"                                        -- GenerateHtmlVariant.hs:174
{-# INLINE address #-}                                                          -- GenerateHtmlVariant.hs:175

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
area :: HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:215
area = open "<area"                                                             -- GenerateHtmlVariant.hs:216
{-# INLINE area #-}                                                             -- GenerateHtmlVariant.hs:217

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
b :: HtmlA   -- ^ Inner HTML.                                                   -- GenerateHtmlVariant.hs:172
  -> HtmlA   -- ^ Resulting HTML.                                               -- GenerateHtmlVariant.hs:173
b = parent "<b" "</b>"                                                          -- GenerateHtmlVariant.hs:174
{-# INLINE b #-}                                                                -- GenerateHtmlVariant.hs:175

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
bdo :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
bdo = parent "<bdo" "</bdo>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE bdo #-}                                                              -- GenerateHtmlVariant.hs:175

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
big :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
big = parent "<big" "</big>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE big #-}                                                              -- GenerateHtmlVariant.hs:175

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
blockquote :: HtmlA   -- ^ Inner HTML.                                          -- GenerateHtmlVariant.hs:172
           -> HtmlA   -- ^ Resulting HTML.                                      -- GenerateHtmlVariant.hs:173
blockquote = parent "<blockquote" "</blockquote>"                               -- GenerateHtmlVariant.hs:174
{-# INLINE blockquote #-}                                                       -- GenerateHtmlVariant.hs:175

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
body :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
body = parent "<body" "</body>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE body #-}                                                             -- GenerateHtmlVariant.hs:175

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
br :: HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:215
br = open "<br"                                                                 -- GenerateHtmlVariant.hs:216
{-# INLINE br #-}                                                               -- GenerateHtmlVariant.hs:217

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
button :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
button = parent "<button" "</button>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE button #-}                                                           -- GenerateHtmlVariant.hs:175

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
caption :: HtmlA   -- ^ Inner HTML.                                             -- GenerateHtmlVariant.hs:172
        -> HtmlA   -- ^ Resulting HTML.                                         -- GenerateHtmlVariant.hs:173
caption = parent "<caption" "</caption>"                                        -- GenerateHtmlVariant.hs:174
{-# INLINE caption #-}                                                          -- GenerateHtmlVariant.hs:175

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
cite :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
cite = parent "<cite" "</cite>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE cite #-}                                                             -- GenerateHtmlVariant.hs:175

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
code :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
code = parent "<code" "</code>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE code #-}                                                             -- GenerateHtmlVariant.hs:175

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
col :: HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:215
col = open "<col"                                                               -- GenerateHtmlVariant.hs:216
{-# INLINE col #-}                                                              -- GenerateHtmlVariant.hs:217

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
colgroup :: HtmlA   -- ^ Inner HTML.                                            -- GenerateHtmlVariant.hs:172
         -> HtmlA   -- ^ Resulting HTML.                                        -- GenerateHtmlVariant.hs:173
colgroup = parent "<colgroup" "</colgroup>"                                     -- GenerateHtmlVariant.hs:174
{-# INLINE colgroup #-}                                                         -- GenerateHtmlVariant.hs:175

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
dd :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
dd = parent "<dd" "</dd>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE dd #-}                                                               -- GenerateHtmlVariant.hs:175

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
del :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
del = parent "<del" "</del>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE del #-}                                                              -- GenerateHtmlVariant.hs:175

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
dfn :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
dfn = parent "<dfn" "</dfn>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE dfn #-}                                                              -- GenerateHtmlVariant.hs:175

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
div :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
div = parent "<div" "</div>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE div #-}                                                              -- GenerateHtmlVariant.hs:175

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
dl :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
dl = parent "<dl" "</dl>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE dl #-}                                                               -- GenerateHtmlVariant.hs:175

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
dt :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
dt = parent "<dt" "</dt>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE dt #-}                                                               -- GenerateHtmlVariant.hs:175

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
em :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
em = parent "<em" "</em>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE em #-}                                                               -- GenerateHtmlVariant.hs:175

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
fieldset :: HtmlA   -- ^ Inner HTML.                                            -- GenerateHtmlVariant.hs:172
         -> HtmlA   -- ^ Resulting HTML.                                        -- GenerateHtmlVariant.hs:173
fieldset = parent "<fieldset" "</fieldset>"                                     -- GenerateHtmlVariant.hs:174
{-# INLINE fieldset #-}                                                         -- GenerateHtmlVariant.hs:175

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
form :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
form = parent "<form" "</form>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE form #-}                                                             -- GenerateHtmlVariant.hs:175

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
h1 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h1 = parent "<h1" "</h1>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h1 #-}                                                               -- GenerateHtmlVariant.hs:175

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
h2 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h2 = parent "<h2" "</h2>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h2 #-}                                                               -- GenerateHtmlVariant.hs:175

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
h3 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h3 = parent "<h3" "</h3>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h3 #-}                                                               -- GenerateHtmlVariant.hs:175

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
h4 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h4 = parent "<h4" "</h4>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h4 #-}                                                               -- GenerateHtmlVariant.hs:175

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
h5 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h5 = parent "<h5" "</h5>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h5 #-}                                                               -- GenerateHtmlVariant.hs:175

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
h6 :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
h6 = parent "<h6" "</h6>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE h6 #-}                                                               -- GenerateHtmlVariant.hs:175

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
head :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
head = parent "<head" "</head>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE head #-}                                                             -- GenerateHtmlVariant.hs:175

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
hr :: HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:215
hr = open "<hr"                                                                 -- GenerateHtmlVariant.hs:216
{-# INLINE hr #-}                                                               -- GenerateHtmlVariant.hs:217

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
htmlNoDocType :: HtmlA   -- ^ Inner HTML.                                       -- GenerateHtmlVariant.hs:172
              -> HtmlA   -- ^ Resulting HTML.                                   -- GenerateHtmlVariant.hs:173
htmlNoDocType = parent "<html" "</html>"                                        -- GenerateHtmlVariant.hs:174
{-# INLINE htmlNoDocType #-}                                                    -- GenerateHtmlVariant.hs:175

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
i :: HtmlA   -- ^ Inner HTML.                                                   -- GenerateHtmlVariant.hs:172
  -> HtmlA   -- ^ Resulting HTML.                                               -- GenerateHtmlVariant.hs:173
i = parent "<i" "</i>"                                                          -- GenerateHtmlVariant.hs:174
{-# INLINE i #-}                                                                -- GenerateHtmlVariant.hs:175

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
img :: HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:215
img = open "<img"                                                               -- GenerateHtmlVariant.hs:216
{-# INLINE img #-}                                                              -- GenerateHtmlVariant.hs:217

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
input :: HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:215
input = open "<input"                                                           -- GenerateHtmlVariant.hs:216
{-# INLINE input #-}                                                            -- GenerateHtmlVariant.hs:217

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
ins :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
ins = parent "<ins" "</ins>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE ins #-}                                                              -- GenerateHtmlVariant.hs:175

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
kbd :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
kbd = parent "<kbd" "</kbd>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE kbd #-}                                                              -- GenerateHtmlVariant.hs:175

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
label :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
label = parent "<label" "</label>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE label #-}                                                            -- GenerateHtmlVariant.hs:175

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
legend :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
legend = parent "<legend" "</legend>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE legend #-}                                                           -- GenerateHtmlVariant.hs:175

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
li :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
li = parent "<li" "</li>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE li #-}                                                               -- GenerateHtmlVariant.hs:175

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
link :: HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:215
link = open "<link"                                                             -- GenerateHtmlVariant.hs:216
{-# INLINE link #-}                                                             -- GenerateHtmlVariant.hs:217

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
map :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
map = parent "<map" "</map>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE map #-}                                                              -- GenerateHtmlVariant.hs:175

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
meta :: HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:215
meta = open "<meta"                                                             -- GenerateHtmlVariant.hs:216
{-# INLINE meta #-}                                                             -- GenerateHtmlVariant.hs:217

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
noscript :: HtmlA   -- ^ Inner HTML.                                            -- GenerateHtmlVariant.hs:172
         -> HtmlA   -- ^ Resulting HTML.                                        -- GenerateHtmlVariant.hs:173
noscript = parent "<noscript" "</noscript>"                                     -- GenerateHtmlVariant.hs:174
{-# INLINE noscript #-}                                                         -- GenerateHtmlVariant.hs:175

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
object :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
object = parent "<object" "</object>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE object #-}                                                           -- GenerateHtmlVariant.hs:175

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
ol :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
ol = parent "<ol" "</ol>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE ol #-}                                                               -- GenerateHtmlVariant.hs:175

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
optgroup :: HtmlA   -- ^ Inner HTML.                                            -- GenerateHtmlVariant.hs:172
         -> HtmlA   -- ^ Resulting HTML.                                        -- GenerateHtmlVariant.hs:173
optgroup = parent "<optgroup" "</optgroup>"                                     -- GenerateHtmlVariant.hs:174
{-# INLINE optgroup #-}                                                         -- GenerateHtmlVariant.hs:175

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
option :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
option = parent "<option" "</option>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE option #-}                                                           -- GenerateHtmlVariant.hs:175

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
p :: HtmlA   -- ^ Inner HTML.                                                   -- GenerateHtmlVariant.hs:172
  -> HtmlA   -- ^ Resulting HTML.                                               -- GenerateHtmlVariant.hs:173
p = parent "<p" "</p>"                                                          -- GenerateHtmlVariant.hs:174
{-# INLINE p #-}                                                                -- GenerateHtmlVariant.hs:175

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
param :: HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:215
param = open "<param"                                                           -- GenerateHtmlVariant.hs:216
{-# INLINE param #-}                                                            -- GenerateHtmlVariant.hs:217

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
pre :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
pre = parent "<pre" "</pre>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE pre #-}                                                              -- GenerateHtmlVariant.hs:175

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
q :: HtmlA   -- ^ Inner HTML.                                                   -- GenerateHtmlVariant.hs:172
  -> HtmlA   -- ^ Resulting HTML.                                               -- GenerateHtmlVariant.hs:173
q = parent "<q" "</q>"                                                          -- GenerateHtmlVariant.hs:174
{-# INLINE q #-}                                                                -- GenerateHtmlVariant.hs:175

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
samp :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
samp = parent "<samp" "</samp>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE samp #-}                                                             -- GenerateHtmlVariant.hs:175

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
script :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
script = parent "<script" "</script>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE script #-}                                                           -- GenerateHtmlVariant.hs:175

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
select :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
select = parent "<select" "</select>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE select #-}                                                           -- GenerateHtmlVariant.hs:175

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
small :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
small = parent "<small" "</small>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE small #-}                                                            -- GenerateHtmlVariant.hs:175

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
span :: HtmlA   -- ^ Inner HTML.                                                -- GenerateHtmlVariant.hs:172
     -> HtmlA   -- ^ Resulting HTML.                                            -- GenerateHtmlVariant.hs:173
span = parent "<span" "</span>"                                                 -- GenerateHtmlVariant.hs:174
{-# INLINE span #-}                                                             -- GenerateHtmlVariant.hs:175

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
strong :: HtmlA   -- ^ Inner HTML.                                              -- GenerateHtmlVariant.hs:172
       -> HtmlA   -- ^ Resulting HTML.                                          -- GenerateHtmlVariant.hs:173
strong = parent "<strong" "</strong>"                                           -- GenerateHtmlVariant.hs:174
{-# INLINE strong #-}                                                           -- GenerateHtmlVariant.hs:175

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
style :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
style = parent "<style" "</style>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE style #-}                                                            -- GenerateHtmlVariant.hs:175

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
sub :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
sub = parent "<sub" "</sub>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE sub #-}                                                              -- GenerateHtmlVariant.hs:175

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
sup :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
sup = parent "<sup" "</sup>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE sup #-}                                                              -- GenerateHtmlVariant.hs:175

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
table :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
table = parent "<table" "</table>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE table #-}                                                            -- GenerateHtmlVariant.hs:175

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
tbody :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
tbody = parent "<tbody" "</tbody>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE tbody #-}                                                            -- GenerateHtmlVariant.hs:175

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
td :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
td = parent "<td" "</td>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE td #-}                                                               -- GenerateHtmlVariant.hs:175

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
textarea :: HtmlA   -- ^ Inner HTML.                                            -- GenerateHtmlVariant.hs:172
         -> HtmlA   -- ^ Resulting HTML.                                        -- GenerateHtmlVariant.hs:173
textarea = parent "<textarea" "</textarea>"                                     -- GenerateHtmlVariant.hs:174
{-# INLINE textarea #-}                                                         -- GenerateHtmlVariant.hs:175

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
tfoot :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
tfoot = parent "<tfoot" "</tfoot>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE tfoot #-}                                                            -- GenerateHtmlVariant.hs:175

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
th :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
th = parent "<th" "</th>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE th #-}                                                               -- GenerateHtmlVariant.hs:175

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
thead :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
thead = parent "<thead" "</thead>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE thead #-}                                                            -- GenerateHtmlVariant.hs:175

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
title :: HtmlA   -- ^ Inner HTML.                                               -- GenerateHtmlVariant.hs:172
      -> HtmlA   -- ^ Resulting HTML.                                           -- GenerateHtmlVariant.hs:173
title = parent "<title" "</title>"                                              -- GenerateHtmlVariant.hs:174
{-# INLINE title #-}                                                            -- GenerateHtmlVariant.hs:175

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
tr :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
tr = parent "<tr" "</tr>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE tr #-}                                                               -- GenerateHtmlVariant.hs:175

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
tt :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
tt = parent "<tt" "</tt>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE tt #-}                                                               -- GenerateHtmlVariant.hs:175

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
ul :: HtmlA   -- ^ Inner HTML.                                                  -- GenerateHtmlVariant.hs:172
   -> HtmlA   -- ^ Resulting HTML.                                              -- GenerateHtmlVariant.hs:173
ul = parent "<ul" "</ul>"                                                       -- GenerateHtmlVariant.hs:174
{-# INLINE ul #-}                                                               -- GenerateHtmlVariant.hs:175

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
var :: HtmlA   -- ^ Inner HTML.                                                 -- GenerateHtmlVariant.hs:172
    -> HtmlA   -- ^ Resulting HTML.                                             -- GenerateHtmlVariant.hs:173
var = parent "<var" "</var>"                                                    -- GenerateHtmlVariant.hs:174
{-# INLINE var #-}                                                              -- GenerateHtmlVariant.hs:175
