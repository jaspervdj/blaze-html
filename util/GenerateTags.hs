-- | Generates code for HTML tags.
--
module GenerateTags where

import Sanitize (sanitize)

-- | Create a string, consisting of @x@ spaces, where @x@ is the length of the
-- argument.
--
spaces :: String -> String
spaces = flip replicate ' ' . length

-- | Generate an export list for a Haskell module.
--
exportList :: String   -- ^ Module name.
           -> [String] -- ^ List of functions.
           -> String   -- ^ Resulting string.
exportList name []            = error "exportList without functions."
exportList name (f:functions) = unlines $
    [ "module " ++ name
    , "    ( " ++ f
    ] ++
    map ("    , " ++) functions ++
    [ "    ) where"]

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
    , function        ++ " :: Html -- ^ Inner HTML."
    , spaces function ++ " -> Html -- ^ Resulting HTML."
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

-- | Generate a function for an HTML attribute.
--
attribute :: String -> String
attribute name = unlines
    [ "-- | Combinator for the @" ++ name ++ "@ attribute."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > img ! " ++ function ++ " \"bar\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <img " ++ name ++ "=\"bar\" />"
    , "--"
    , function        ++ " :: Text      -- ^ Attribute value."
    , spaces function ++ " -> Attribute -- ^ Resulting attribute."
    , function ++ " = attribute \"" ++ name ++ "\""
    ]
  where
    function = sanitize name

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
