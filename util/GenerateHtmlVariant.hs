{-# LANGUAGE CPP #-}

#define LINE codeLine __FILE__ __LINE__

-- | Generates code for HTML tags.
--
module GenerateHtmlVariant where

import Sanitize (sanitize)

import Data.List (isPrefixOf, sort, sortBy)
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

codeLine :: String -> Int -> String -> String
codeLine filename lineNo line
    | "--" `isPrefixOf` line = line
    | otherwise = line ++ (replicate (79 - length line) ' ') ++ " -- " ++ filename ++ ":" ++ show lineNo

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
        [ LINE "{-# LANGUAGE OverloadedStrings #-}"
        , LINE "-- | This module exports HTML combinators used to create documents."
        , LINE "--"
        , exportList modulName $ "module Text.Blaze"
                                : "html"
                                : "docType"
                                : (map (sanitize . fst) sortedTags)
        , LINE "import Prelude ((>>))"
        , LINE ""
        , LINE "import Text.Blaze"
        , LINE "import Text.Blaze.Internal (parent, leaf, open)"
        , LINE ""
        , makeHtml $ docType htmlVariant
        , makeDocType $ docType htmlVariant
        , unlines appliedTags
        ]

    let sortedAttributes = sort attributes'

    -- Write the attribute module.
    writeFile (basePath </> "Attributes.hs") $ removeTrailingNewlines $ unlines
        [ LINE "{-# LANGUAGE OverloadedStrings #-}"
        , LINE "-- | This module exports combinators that provide you with the"
        , LINE "-- ability to set attributes on HTML elements."
        , LINE "--"
        , exportList attributeModuleName $ map sanitize sortedAttributes
        , LINE "import Prelude ()"
        , LINE ""
        , LINE "import Data.Text (Text)"
        , LINE ""
        , LINE "import Text.Blaze.Internal (Attribute, AttributeValue, attribute)"
        , LINE ""
        , unlines (map makeAttribute sortedAttributes)
        ]
  where
    basePath = "src" </> "Text" </> "Blaze" </> foldl1 (</>) version'
    modulName = "Text.Blaze." ++ intercalate "." version' -- YAIRCHU: renamed moduleName to modulName to work-around a TextMate syntax highlighting bug
    attributeModuleName = modulName ++ ".Attributes"
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
    [ LINE $ "module " ++ name
    , LINE $ "    ( " ++ sanitize f
    ] ++ 
    map (LINE . ("    , " ++)) functions ++
    [ LINE "    ) where"]

-- | Generate a function for a doctype.
--
makeDocType :: [String] -> String
makeDocType lines' = unlines
    [ LINE "-- | Combinator for the document type. This should be placed at the top"
    , LINE "-- of every HTML page."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE "-- > docType"
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , unlines (map ("-- > " ++) lines') ++ "--"
    , LINE "docType :: HtmlA   -- ^ The document type HTML."
    , LINE $ "docType = preEscapedText " ++ (show $ unlines lines')
    , LINE "{-# INLINE docType #-}"
    ]

-- | Generate a function for the HTML tag (including the doctype).
--
makeHtml :: [String]  -- ^ The doctype.
         -> String    -- ^ Resulting combinator function.
makeHtml lines' = unlines
    [ LINE "-- | Combinator for the @\\<html>@ element. This combinator will also"
    , LINE "-- insert the correct doctype."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE "-- > html $ span $ text \"foo\""
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ unlines (map ("-- > " ++) lines') ++ "-- > <html><span>foo</span></html>"
    , LINE "--"
    , LINE "html :: HtmlA   -- ^ Inner HTML."
    , LINE "     -> HtmlA   -- ^ Resulting HTML."
    , LINE "html inner = docType >> htmlNoDocType inner"
    , LINE "{-# INLINE html #-}"
    ]

-- | Generate a function for an HTML tag that can be a parent.
--
makeParent :: String -> String
makeParent tag = unlines
    [ LINE $ "-- | Combinator for the @\\<" ++ tag ++ ">@ element."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE $ "-- > " ++ function ++ " $ span $ text \"foo\""
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ "-- > <" ++ tag ++ "><span>foo</span></" ++ tag ++ ">"
    , LINE "--"
    , LINE $ function        ++ " :: HtmlA   -- ^ Inner HTML."
    , LINE $ spaces function ++ " -> HtmlA   -- ^ Resulting HTML."
    , LINE $ function ++ " = parent \"<" ++ tag ++ "\" \"</" ++ tag ++ ">\""
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML tag that must be a leaf.
--
makeLeaf :: String -> String
makeLeaf tag = unlines
    [ LINE $ "-- | Combinator for the @\\<" ++ tag ++ " />@ element."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE $ "-- > " ++ function
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ "-- > <" ++ tag ++ " />"
    , LINE "--"
    , LINE $ function        ++ " :: HtmlA   -- ^ Resulting HTML."
    , LINE $ function ++ " = leaf \"<" ++ tag ++ "\""
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML tag that must be open.
--
makeOpen :: String -> String
makeOpen tag = unlines
    [ LINE $ "-- | Combinator for the @\\<" ++ tag ++ ">@ element."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE $ "-- > " ++ function
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ "-- > <" ++ tag ++ ">"
    , LINE "--"
    , LINE $ function        ++ " :: HtmlA   -- ^ Resulting HTML."
    , LINE $ function ++ " = open \"<" ++ tag ++ "\""
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML attribute.
--
makeAttribute :: String -> String
makeAttribute name = unlines
    [ LINE $ "-- | Combinator for the @" ++ name ++ "@ attribute."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE $ "-- > div ! " ++ function ++ " \"bar\" $ \"Hello.\""
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ "-- > <div " ++ name ++ "=\"bar\">Hello.</div>"
    , LINE "--"
    , LINE $ function        ++ " :: AttributeValue  -- ^ Attribute value."
    , LINE $ spaces function ++ " -> Attribute       -- ^ Resulting attribute."
    , LINE $ function ++ " = attribute \" " ++ name ++ "=\\\"\""
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
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
