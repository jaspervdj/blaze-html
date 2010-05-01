-- | Generates code for HTML tags.
--
module GenerateTags where

import Sanitize (sanitize)

import Data.List (sort, sortBy)
import Data.List (intersperse, intercalate)
import Data.Ord (comparing)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

-- | Datatype for an HTML variant.
--
data HtmlVariant = HtmlVariant
    { version    :: [String]
    , parents    :: [String]
    , leafs      :: [String]
    , attributes :: [String]
    } deriving (Show)

-- | Write an HTML variant.
--
writeHtmlVariant :: HtmlVariant -> IO ()
writeHtmlVariant htmlVariant = do
    -- Make a directory.
    createDirectoryIfMissing True $ basePath

    let tags = zip parents' (repeat parent) ++ zip leafs' (repeat leaf)
        sortedTags = sortBy (comparing fst) tags
        appliedTags = map (\(x, f) -> f x) sortedTags

    -- Write the main module.
    writeFile (basePath <.> "hs") $ removeTrailingNewlines $ unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , exportList moduleName (map fst sortedTags)
        , "import Prelude ()"
        , ""
        , "import Data.Text (Text)"
        , "import Data.ByteString.Char8 (ByteString)"
        , ""
        , "import Text.Blaze (Html, parent, leaf)"
        , ""
        , unlines appliedTags
        ]

    let sortedAttributes = sort attributes'

    -- Write the attribute module.
    writeFile (basePath </> "Attributes.hs") $ removeTrailingNewlines $ unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , exportList attributeModuleName sortedAttributes
        , "import Prelude ()"
        , ""
        , "import Data.Text (Text)"
        , "import Data.ByteString.Char8 (ByteString)"
        , ""
        , "import Text.Blaze (Attribute, attribute)"
        , ""
        , unlines (map attribute sortedAttributes)
        ]
  where
    basePath = "src" </> "Text" </> "Blaze" </> foldl1 (</>) version'
    moduleName = "Text.Blaze." ++ intercalate "." version'
    attributeModuleName = moduleName ++ ".Attributes"
    attributes' = attributes htmlVariant
    parents'    = parents htmlVariant
    leafs'      = leafs htmlVariant
    version'    = version htmlVariant
    removeTrailingNewlines = reverse . drop 2 . reverse

-- | Create a string, consisting of @x@ spaces, where @x@ is the length of the
-- argument.
--
spaces :: String -> String
spaces = flip replicate ' ' . length

-- | Join blocks of code with a newline in between.
--
unblocks :: [String] -> String
unblocks = unlines . intersperse "\n"

-- | Generate an export list for a Haskell module.
--
exportList :: String   -- ^ Module name.
           -> [String] -- ^ List of tags.
           -> String   -- ^ Resulting string.
exportList _    []            = error "exportList without functions."
exportList name (f:functions) = unlines $
    [ "module " ++ name
    , "    ( " ++ sanitize f
    ] ++
    map (("    , " ++) . sanitize) functions ++
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
    , "    in parent begin end"
    , "{-# INLINE " ++ function ++ "#-}"
    ]
  where
    function = sanitize tag


-- | Generate a function for an HTML tag that must be a leaf.
--
leaf :: String -> String
leaf tag = unlines
    [ "-- | Combinator for the @<" ++ tag ++ " />@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ " />"
    , "--"
    , function        ++ " :: Html -- ^ Resulting HTML."
    , function ++ " ="
    , "    let begin :: ByteString"
    , "        begin = \"<" ++ tag ++ "\""
    , "        {-# NOINLINE begin #-}"
    , "    in leaf begin"
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

-- | HTML 4.01 Strict.
-- A good reference can be found here: http://www.w3schools.com/tags/default.asp
html4Strict :: HtmlVariant
html4Strict = HtmlVariant
    { version = ["Html4", "Strict"]
    , parents = 
        [ "a", "abbr", "acronym", "address", "b", "bdo", "big", "blockquote"
        , "body" , "button", "caption", "cite", "code", "colgroup", "dd", "del"
        , "dfn", "div" , "dl", "dt", "em", "fieldset", "form", "h1", "h2", "h3"
        , "h4", "h5", "h6", "head", "html", "i", "ins" , "kbd", "label"
        , "legend", "li", "map", "noscript", "object", "ol", "optgroup"
        , "option", "p", "pre", "q", "samp", "script", "select", "small"
        , "span", "strong", "style", "sub", "sup", "table", "tbody", "td"
        , "textarea", "tfoot", "th", "thead", "title", "tr", "tt", "ul", "var"
        ]
    , leafs = 
        [ "img", "input", "area", "br", "col", "hr", "link", "meta", "param"
        ]
    , attributes = 
        [ "abbr", "accept", "accesskey", "action", "align", "alt", "archive"
        , "axis", "border", "cellpadding", "cellspacing", "char", "charoff"
        , "charset", "checked", "cite", "class", "classid", "codebase"
        , "codetype", "cols", "colspan", "content", "coords", "data", "datetime"
        , "declare", "defer", "dir", "disabled", "for", "frame", "headers"
        , "height", "href", "hreflang", "http-equiv", "id", "label", "lang"
        , "maxlength", "media", "multiple", "name", "nohref", "onabort"
        , "onblur", "onchange", "onclick", "ondblclick", "onfocus", "onkeydown"
        , "onkeypress", "onkeyup", "onload", "onmousedown", "onmousemove"
        , "onmouseout", "onmouseover", "onmouseup", "onreset", "onselect"
        , "onsubmit", "onunload", "profile", "readonly", "rel", "rev", "rows"
        , "rowspan", "rules", "scheme", "scope", "selected", "shape", "size"
        , "span", "src", "standby", "style", "summary", "tabindex", "title"
        , "type", "usemap", "valign", "value", "valuetype", "width"
        ]
    }

main = do
    writeHtmlVariant html4Strict
