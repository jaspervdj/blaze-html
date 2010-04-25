-- | Generates code for HTML tags.
--
module GenerateTags where

import Sanitize (sanitize)

-- | Generate a function for an HTML tag that can be a parent.
--
parent :: String -> String
parent tag = unlines
    [ "-- | Combinator for the @<" ++ tag ++ ">@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function ++ " $ text \"foo\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ ">foo</" ++ tag ++ ">"
    , "--"
    , function         ++ " :: Html -- ^ Inner HTML."
    , justify function ++ " -> Html -- ^ Resulting HTML."
    , function ++ " ="
    , "    let begin, end :: ByteString"
    , "        begin = \"<" ++ tag ++ "\""
    , "        end = \"</" ++ tag ++ ">\""
    , "        {-# NOINLINE begin #-}"
    , "        {-# NOINLINE end #-}"
    , "    in tag begin end"
    , "{-# INLINE " ++ function ++ "#-}"
    ]
  where
    function = sanitize tag
    justify = flip replicate ' ' . length

-- | A list of HTML strict non-leaf nodes.
--
strictParents :: [String]
strictParents =
    [ "a", "abbr", "acronym", "address", "b", "bdo", "big", "blockquote", "body"
    , "button", "caption", "cite", "code", "colgroup", "dd", "del", "dfn", "div"
    , "dl", "dt", "em", "fieldset", "form", "h1", "head", "html", "i", "ins"
    , "kbd", "label", "legend", "li", "map", "noscript", "object", "ol"
    , "optgroup", "option", "p", "pre", "q", "samp", "script", "select", "small"
    , "span", "strong", "style", "sub", "sup", "table", "tbody", "td"
    , "textarea", "tfoot", "th", "thead", "title", "tr", "tt", "ul", "var"
    ]

-- | A list of HTML strict leaf nodes.
--
strictLeafs :: [String]
strictLeafs =
    [ "img", "input", "area", "br", "col", "hr", "link", "meta", "param"
    ]
