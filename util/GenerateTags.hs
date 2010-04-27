-- | Generates code for HTML tags.
--
module GenerateTags where

import Sanitize (sanitize)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Data.List (sort, sortBy)
import Data.List (intersperse)
import Data.Ord (comparing)

-- | Datatype for an HTML variant.
--
data HtmlVariant = HtmlVariant
    { attributes :: [String]
    , parents    :: [String]
    , leafs      :: [String]
    , version    :: String
    , variant    :: String
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
        [ exportList moduleName (map fst sortedTags)
        , unlines appliedTags
        ]

    let sortedAttributes = sort attributes'

    -- Write the attribute module.
    writeFile (basePath </> "Attributes.hs") $ removeTrailingNewlines $ unlines
        [ exportList attributeModuleName sortedAttributes
        , unlines (map attribute sortedAttributes)
        ]
  where
    basePath = "src" </> "Text" </> "Blaze" </> version' </> variant'
    moduleName = "Text.Blaze." ++ version' ++ "." ++ variant'
    attributeModuleName = moduleName ++ ".Attributes"
    attributes' = attributes htmlVariant
    parents'    = parents htmlVariant
    leafs'      = leafs htmlVariant
    version'    = version htmlVariant
    variant'    = variant htmlVariant
    removeTrailingNewlines = reverse . drop 2 . reverse

test = writeHtmlVariant $ HtmlVariant
    { attributes = ["id", "class"]
    , parents = ["parent", "div"]
    , leafs = ["leaf"]
    , version = "Html5"
    , variant = "Transitional"
    }

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
    , "    in tag begin end"
    , "{-# INLINE " ++ function ++ "#-}"
    ]
  where
    function = sanitize tag


-- | Generate a function for an HTML tag that must be a leaf.
--
leaf :: String -> String
leaf = const "foo"

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
