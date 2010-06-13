-- | Generates code for HTML tags.
--
module GenerateHtmlVariant where

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
    , docType    :: [String]
    , parents    :: [String]
    , leafs      :: [String]
    , opens      :: [String]
    , attributes :: [String]
    } deriving (Show)

-- | Write an HTML variant.
--
writeHtmlVariant :: HtmlVariant -> IO ()
writeHtmlVariant htmlVariant = do
    -- Make a directory.
    createDirectoryIfMissing True $ basePath

    let tags =  zip parents' (repeat makeParent)
             ++ zip leafs' (repeat makeLeaf)
             ++ zip opens' (repeat makeOpen)
        sortedTags = sortBy (comparing fst) tags
        appliedTags = map (\(x, f) -> f x) sortedTags

    -- Write the main module.
    writeFile (basePath <.> "hs") $ removeTrailingNewlines $ unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "-- | This module exports HTML combinators used to create documents."
        , "--"
        , exportList moduleName $ "module Text.Blaze"
                                : "html"
                                : "docType"
                                : (map (sanitize . fst) sortedTags)
        , "import Prelude ()"
        , "import Data.Monoid (mappend)"
        , ""
        , "import Text.Blaze"
        , "import Text.Blaze.Internal (parent, leaf, open)"
        , ""
        , makeHtml $ docType htmlVariant
        , makeDocType $ docType htmlVariant
        , unlines appliedTags
        ]

    let sortedAttributes = sort attributes'

    -- Write the attribute module.
    writeFile (basePath </> "Attributes.hs") $ removeTrailingNewlines $ unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "-- | This module exports combinators that provide you with the"
        , "-- ability to set attributes on HTML elements."
        , "--"
        , exportList attributeModuleName $ map sanitize sortedAttributes
        , "import Prelude ()"
        , ""
        , "import Data.Text (Text)"
        , ""
        , "import Text.Blaze.Internal (Attribute, AttributeValue, attribute)"
        , ""
        , unlines (map makeAttribute sortedAttributes)
        ]
  where
    basePath = "src" </> "Text" </> "Blaze" </> foldl1 (</>) version'
    moduleName = "Text.Blaze." ++ intercalate "." version'
    attributeModuleName = moduleName ++ ".Attributes"
    attributes' = attributes htmlVariant
    parents'    = parents htmlVariant
    leafs'      = leafs htmlVariant
    opens'      = opens htmlVariant
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
           -> [String] -- ^ List of functions.
           -> String   -- ^ Resulting string.
exportList _    []            = error "exportList without functions."
exportList name (f:functions) = unlines $
    [ "module " ++ name
    , "    ( " ++ sanitize f
    ] ++
    map ("    , " ++) functions ++
    [ "    ) where"]

-- | Generate a function for a doctype.
--
makeDocType :: [String] -> String
makeDocType lines' = unlines
    [ "-- | Combinator for the document type. This should be placed at the top"
    , "-- of every HTML page."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > docType"
    , "--"
    , "-- Result:"
    , "--"
    , unlines (map ("-- > " ++) lines') ++ "--"
    , "docType :: Html a  -- ^ The document type HTML."
    , "docType = preEscapedText " ++ (show $ unlines lines')
    , "{-# INLINE docType #-}"
    ]

-- | Generate a function for the HTML tag (including the doctype).
--
makeHtml :: [String]  -- ^ The doctype.
         -> String    -- ^ Resulting combinator function.
makeHtml lines' = unlines
    [ "-- | Combinator for the @\\<html>@ element. This combinator will also"
    , "-- insert the correct doctype."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > html $ span $ text \"foo\""
    , "--"
    , "-- Result:"
    , "--"
    , unlines (map ("-- > " ++) lines') ++ "-- > <html><span>foo</span></html>"
    , "--"
    , "html :: Html a  -- ^ Inner HTML."
    , "     -> Html b  -- ^ Resulting HTML."
    , "html inner = docType `mappend` htmlNoDocType inner"
    , "{-# INLINE html #-}"
    ]

-- | Generate a function for an HTML tag that can be a parent.
--
makeParent :: String -> String
makeParent tag = unlines
    [ "-- | Combinator for the @\\<" ++ tag ++ ">@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function ++ " $ span $ text \"foo\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ "><span>foo</span></" ++ tag ++ ">"
    , "--"
    , function        ++ " :: Html a  -- ^ Inner HTML."
    , spaces function ++ " -> Html b  -- ^ Resulting HTML."
    , function ++ " = parent \"<" ++ tag ++ "\" \"</" ++ tag ++ ">\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML tag that must be a leaf.
--
makeLeaf :: String -> String
makeLeaf tag = unlines
    [ "-- | Combinator for the @\\<" ++ tag ++ " />@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ " />"
    , "--"
    , function        ++ " :: Html a  -- ^ Resulting HTML."
    , function ++ " = leaf \"<" ++ tag ++ "\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML tag that must be open.
--
makeOpen :: String -> String
makeOpen tag = unlines
    [ "-- | Combinator for the @\\<" ++ tag ++ ">@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ ">"
    , "--"
    , function        ++ " :: Html a  -- ^ Resulting HTML."
    , function ++ " = open \"<" ++ tag ++ "\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML attribute.
--
makeAttribute :: String -> String
makeAttribute name = unlines
    [ "-- | Combinator for the @" ++ name ++ "@ attribute."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > div <! " ++ function ++ " \"bar\" $ \"Hello.\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <div " ++ name ++ "=\"bar\">Hello.</div>"
    , "--"
    , function        ++ " :: AttributeValue  -- ^ Attribute value."
    , spaces function ++ " -> Attribute       -- ^ Resulting attribute."
    , function ++ " = attribute \" " ++ name ++ "=\\\"\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize name

-- | HTML 4.01 Strict.
-- A good reference can be found here: http://www.w3schools.com/tags/default.asp
--
html4Strict :: HtmlVariant
html4Strict = HtmlVariant
    { version = ["Html4", "Strict"]
    , docType =
        [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""
        , "    \"http://www.w3.org/TR/html4/strict.dtd\">"
        ]
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
    , leafs = []
    , opens = 
        [ "area", "br", "col", "hr", "link", "img", "input",  "meta", "param"
        ]
    , attributes = 
        [ "abbr", "accept", "accesskey", "action", "align", "alt", "archive"
        , "axis", "border", "cellpadding", "cellspacing", "char", "charoff"
        , "charset", "checked", "cite", "class", "classid", "codebase"
        , "codetype", "cols", "colspan", "content", "coords", "data", "datetime"
        , "declare", "defer", "dir", "disabled", "for", "frame", "headers"
        , "height", "href", "hreflang", "http-equiv", "id", "label", "lang"
        , "maxlength", "media", "method", "multiple", "name", "nohref"
        , "onabort", "onblur", "onchange", "onclick", "ondblclick", "onfocus"
        , "onkeydown", "onkeypress", "onkeyup", "onload", "onmousedown"
        , "onmousemove", "onmouseout", "onmouseover", "onmouseup", "onreset"
        , "onselect", "onsubmit", "onunload", "profile", "readonly", "rel"
        , "rev", "rows", "rowspan", "rules", "scheme", "scope", "selected"
        , "shape", "size", "span", "src", "standby", "style", "summary"
        , "tabindex", "title", "type", "usemap", "valign", "value", "valuetype"
        , "width"
        ]
    }

-- | HTML 5.0
-- A good reference can be found here:
-- http://www.w3schools.com/html5/html5_reference.asp
--
html5 :: HtmlVariant
html5 = HtmlVariant
    { version = ["Html5"]
    , docType = ["<!DOCTYPE HTML>"]
    , parents =
        [ "a", "abbr", "address", "article", "aside", "audio", "b", "base"
        , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
        , "code", "colgroup", "command", "datalist", "dd", "del", "details"
        , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
        , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
        , "hgroup", "html", "i", "iframe", "ins", "keygen", "kbd", "label"
        , "legend", "li", "map", "mark", "menu", "meter", "nav", "noscript"
        , "object", "ol", "optgroup", "option", "output", "p", "pre", "progress"
        , "q", "rp", "rt", "ruby", "samp", "script", "section", "select"
        , "small", "source", "span", "strong", "style", "sub", "summary", "sup"
        , "table", "tbody", "td", "textarea", "tfoot", "th", "thead", "time"
        , "title", "tr", "ul", "var", "video"
        ]
    , leafs =
        [ "area", "br", "col", "embed", "hr", "img", "input", "meta", "link"
        , "param"
        ]
    , opens = []
    , attributes = 
        [ "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype", "for"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onafterprint", "onbeforeonload", "onbeforeprint"
        , "onblur", "onerror", "onfocus", "onhaschange", "onload", "onmessage"
        , "onoffline", "ononline", "onpagehide", "onpageshow", "onpropstate"
        , "onredo", "onresize", "onstorage", "onundo", "onunload", "open"
        , "optimum", "pattern", "ping", "placeholder", "preload", "pubdate"
        , "radiogroup", "readonly", "rel", "required", "reversed", "rows"
        , "rowspan", "sandbox", "scope", "scoped", "seamless", "selected"
        , "shape", "size", "sizes", "span", "spellcheck", "src", "srcdoc"
        , "start", "step", "style", "subject", "summary", "tabindex", "target"
        , "title", "type", "usemap", "value", "width", "wrap", "xmlns"
        ]
    }

main :: IO ()
main = do
    writeHtmlVariant html4Strict
    writeHtmlVariant html5
