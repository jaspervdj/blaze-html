{-# LANGUAGE CPP #-}

#define LINE codeLine __FILE__ __LINE__

-- | Generates code for HTML tags.
--
module Util.GenerateHtmlCombinators where

import Control.Arrow ((&&&))
import Data.List (isPrefixOf, sort, sortBy, intersperse, intercalate)
import Data.Ord (comparing)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (toLower)
import qualified Data.Set as S

import Util.Sanitize (sanitize, prelude)

-- | Datatype for an HTML variant.
--
data HtmlVariant = HtmlVariant
    { version    :: [String]
    , docType    :: [String]
    , parents    :: [String]
    , leafs      :: [String]
    , attributes :: [String]
    , openLeafs  :: Bool
    } deriving (Eq)

instance Show HtmlVariant where
    show = map toLower . intercalate "-" . version

-- | Get the full module name for an HTML variant.
--
getModuleName :: HtmlVariant -> String
getModuleName = ("Text.Blaze." ++) . intercalate "." . version

-- | Get the attribute module name for an HTML variant.
--
getAttributeModuleName :: HtmlVariant -> String
getAttributeModuleName = (++ ".Attributes") . getModuleName

-- | Check if a given name causes a name clash.
--
isNameClash :: HtmlVariant -> String -> Bool
isNameClash v t
    -- Both an element and an attribute
    | (t `elem` parents v || t `elem` leafs v) && t `elem` attributes v = True
    -- Already a prelude function
    | sanitize t `S.member` prelude = True
    | otherwise = False

-- | Annotate a line of code with the line by which it was generated.
--
codeLine :: String -> Int -> String -> String
codeLine filename lineNo line
    | "--" `isPrefixOf` line = line
    | otherwise = line ++ replicate (79 - length line) ' '
                       ++ " -- " ++ filename ++ ":" ++ show lineNo

-- | Write an HTML variant.
--
writeHtmlVariant :: HtmlVariant -> IO ()
writeHtmlVariant htmlVariant = do
    -- Make a directory.
    createDirectoryIfMissing True basePath

    let tags =  zip parents' (repeat makeParent)
             ++ zip leafs' (repeat $ makeLeaf $ openLeafs htmlVariant)
        sortedTags = sortBy (comparing fst) tags
        appliedTags = map (\(x, f) -> f x) sortedTags

    -- Write the main module.
    writeFile' (basePath <.> "hs") $ removeTrailingNewlines $ unlines
        [ doNotEdit
        , LINE "{-# LANGUAGE OverloadedStrings #-}"
        , LINE "-- | This module exports HTML combinators used to create documents."
        , LINE "--"
        , exportList modulName $ "module Text.Blaze"
                                : "docType"
                                : "docTypeHtml"
                                : map (sanitize . fst) sortedTags
        , LINE "import Prelude ((>>), (.))"
        , LINE ""
        , LINE "import Text.Blaze"
        , LINE "import Text.Blaze.Internal"
        , LINE ""
        , makeDocType $ docType htmlVariant
        , makeDocTypeHtml $ docType htmlVariant
        , unlines appliedTags
        ]

    let sortedAttributes = sort attributes'

    -- Write the attribute module.
    writeFile' (basePath </> "Attributes.hs") $ removeTrailingNewlines $ unlines
        [ doNotEdit
        , LINE "-- | This module exports combinators that provide you with the"
        , LINE "-- ability to set attributes on HTML elements."
        , LINE "--"
        , LINE "{-# LANGUAGE OverloadedStrings #-}"
        , exportList attributeModuleName $ map sanitize sortedAttributes
        , LINE "import Prelude ()"
        , LINE ""
        , LINE "import Text.Blaze.Internal (Attribute, AttributeValue, attribute)"
        , LINE ""
        , unlines (map makeAttribute sortedAttributes)
        ]
  where
    basePath = "Text" </> "Blaze" </> foldl1 (</>) version'
    modulName = getModuleName htmlVariant
    attributeModuleName = getAttributeModuleName htmlVariant
    attributes' = attributes htmlVariant
    parents'    = parents htmlVariant
    leafs'      = leafs htmlVariant
    version'    = version htmlVariant
    removeTrailingNewlines = reverse . drop 2 . reverse
    writeFile' file content = do
        putStrLn ("Generating " ++ file)
        writeFile file content

-- | Create a string, consisting of @x@ spaces, where @x@ is the length of the
-- argument.
--
spaces :: String -> String
spaces = flip replicate ' ' . length

-- | Join blocks of code with a newline in between.
--
unblocks :: [String] -> String
unblocks = unlines . intersperse "\n"

-- | A warning to not edit the generated code.
--
doNotEdit :: String
doNotEdit = unlines
    [ "-- WARNING: This code was automatically generated. You should *never*"
    , "-- edit it directly. Instead, edit the files who generated this code,"
    , "-- you can find them in the @util/@ directory."
    ]

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
    , LINE "docType :: Html  -- ^ The document type HTML."
    , LINE $ "docType = preEscapedText " ++ show (unlines lines')
    , LINE "{-# INLINE docType #-}"
    ]

-- | Generate a function for the HTML tag (including the doctype).
--
makeDocTypeHtml :: [String]  -- ^ The doctype.
                -> String    -- ^ Resulting combinator function.
makeDocTypeHtml lines' = unlines
    [ LINE "-- | Combinator for the @\\<html>@ element. This combinator will also"
    , LINE "-- insert the correct doctype."
    , LINE "--"
    , LINE "-- Example:"
    , LINE "--"
    , LINE "-- > docTypeHtml $ span $ text \"foo\""
    , LINE "--"
    , LINE "-- Result:"
    , LINE "--"
    , LINE $ unlines (map ("-- > " ++) lines') ++ "-- > <html><span>foo</span></html>"
    , LINE "--"
    , LINE "docTypeHtml :: Html  -- ^ Inner HTML."
    , LINE "            -> Html  -- ^ Resulting HTML."
    , LINE "docTypeHtml inner = docType >> html inner"
    , LINE "{-# INLINE docTypeHtml #-}"
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
    , LINE $ function        ++ " :: Html  -- ^ Inner HTML."
    , LINE $ spaces function ++ " -> Html  -- ^ Resulting HTML."
    , LINE $ function ++ " = Parent \"<" ++ tag
                      ++ "\" \"</" ++ tag ++ ">\"" ++ modifier
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag
    modifier = if tag `elem` ["style", "script"] then " . external" else ""

-- | Generate a function for an HTML tag that must be a leaf.
--
makeLeaf :: Bool    -- ^ If we should use open leafs
         -> String  -- ^ Tag for the combinator
         -> String  -- ^ Combinator code
makeLeaf openLeaf tag = unlines
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
    , LINE $ function        ++ " :: Html  -- ^ Resulting HTML."
    , LINE $ function ++ " = Leaf \"<" ++ tag ++ "\" " ++ end
    , LINE $ "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag
    end = if openLeaf then "\">\"" else "\" />\""

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
    , leafs =
        [ "area", "br", "col", "hr", "link", "img", "input",  "meta", "param"
        ]
    , attributes =
        [ "abbr", "accept", "accesskey", "action", "align", "alt", "archive"
        , "axis", "border", "cellpadding", "cellspacing", "char", "charoff"
        , "charset", "checked", "cite", "class", "classid", "codebase"
        , "codetype", "cols", "colspan", "content", "coords", "data", "datetime"
        , "declare", "defer", "dir", "disabled", "enctype", "for", "frame"
        , "headers", "height", "href", "hreflang", "http-equiv", "id", "label"
        , "lang", "maxlength", "media", "method", "multiple", "name", "nohref"
        , "onabort", "onblur", "onchange", "onclick", "ondblclick", "onfocus"
        , "onkeydown", "onkeypress", "onkeyup", "onload", "onmousedown"
        , "onmousemove", "onmouseout", "onmouseover", "onmouseup", "onreset"
        , "onselect", "onsubmit", "onunload", "profile", "readonly", "rel"
        , "rev", "rows", "rowspan", "rules", "scheme", "scope", "selected"
        , "shape", "size", "span", "src", "standby", "style", "summary"
        , "tabindex", "title", "type", "usemap", "valign", "value", "valuetype"
        , "width"
        ]
    , openLeafs = True
    }

-- | HTML 4.0 Transitional
--
html4Transitional :: HtmlVariant
html4Transitional = HtmlVariant
    { version = ["Html4", "Transitional"]
    , docType =
        [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
        , "    \"http://www.w3.org/TR/html4/loose.dtd\">"
        ]
    , parents = parents html4Strict ++
        [ "applet", "center", "dir", "font", "iframe", "isindex", "menu"
        , "noframes", "s", "u"
        ]
    , leafs = leafs html4Strict ++ ["basefont"]
    , attributes = attributes html4Strict ++
        [ "background", "bgcolor", "clear", "compact", "hspace", "language"
        , "noshade", "nowrap", "start", "target", "vspace"
        ]
    , openLeafs = True
    }

-- | HTML 4.0 Frameset
--
html4FrameSet :: HtmlVariant
html4FrameSet = HtmlVariant
    { version = ["Html4", "FrameSet"]
    , docType =
        [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
        , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
        ]
    , parents = parents html4Transitional ++ ["frameset"]
    , leafs = leafs html4Transitional ++ ["frame"]
    , attributes = attributes html4Transitional ++
        [ "frameborder", "scrolling"
        ]
    , openLeafs = True
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
        , "novalidate", "onbeforeonload", "onbeforeprint", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "pubdate", "radiogroup", "readonly", "rel"
        , "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "scoped", "seamless", "selected", "shape", "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        ]
    , openLeafs = False
    }

-- | A map of HTML variants, per version, lowercase.
--
htmlVariants :: Map String HtmlVariant
htmlVariants = M.fromList $ map (show &&& id)
    [ html4Strict
    , html4Transitional
    , html4FrameSet
    , html5
    ]

generateHtmlCombinators :: IO ()
generateHtmlCombinators = mapM_ (writeHtmlVariant . snd) $ M.toList htmlVariants
