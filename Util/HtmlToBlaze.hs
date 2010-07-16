-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module HtmlToBlaze where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)

import Text.HTML.TagSoup

import Util.GenerateHtmlCombinators

-- | Intermediate tree representation
--
data Html = Parent String [(String, String)] [Html]
          | Text String
          | Comment String
          | Doctype
          deriving (Show)

-- | Traverse the list of tags to produce an intermediate representation of the
-- HTML tree.
--
parseHtml :: [String]                -- ^ Stack of open tags
          -> [Tag String]            -- ^ Tags to parse
          -> ([Html], [Tag String])  -- ^ (Result, unparsed part)
parseHtml stack []
    | null stack = ([], [])
    | otherwise = error $ "Error: tags left open at the end: " ++ show stack
parseHtml stack (TagPosition row column : x : xs) = case x of
    TagOpen tag attrs -> if toLower' tag == "!doctype"
        then addHtml Doctype xs
        else let (inner, t) = parseHtml (tag : stack) xs
                 p = Parent (toLower' tag) (map (first toLower') attrs) inner
             in addHtml p t
    -- The closing tag must match the stack.
    TagClose tag -> if listToMaybe stack == Just (toLower' tag)
        then ([], xs)
        else error $  "Line " ++ show row ++ ": " ++ show tag ++ " closed but "
                   ++ show stack ++ " should be closed instead."
    TagText text -> addHtml (Text text) xs
    TagComment comment -> addHtml (Comment comment) xs
    _ -> parseHtml stack xs
  where
    addHtml html xs' = let (l, r) = parseHtml stack xs'
                       in (html : l, r)

    toLower' = map toLower

-- | Remove empty text from the HTML.
--
removeEmptyText :: [Html] -> [Html]
removeEmptyText = map go . filter (not . isEmpty)
  where
    isEmpty (Text text) = all isSpace text
    isEmpty _           = False

    go (Parent tag attrs inner) = Parent tag attrs $ removeEmptyText inner
    go x = x

-- | Convert the HTML to blaze code.
--
toBlaze :: Int -> [Html] -> String
toBlaze indent = intercalate "\n"
               . map (replicate (indent * 4) ' ' ++) . map toBlaze'
  where
    toBlaze' (Text text) = show text
    toBlaze' (Comment comment) = "-- " ++ comment
    toBlaze' Doctype = "docType"
    toBlaze' (Parent tag _ inner)
        | length inner == 1 = tag ++ " $ "
                                  ++ dropWhile isSpace (toBlaze indent inner)
        | otherwise = tag ++ " $\n" ++ toBlaze (indent + 1) inner

parse' :: String -> [Tag String]
parse' = parseTagsOptions parseOptions { optTagPosition = True }

test :: String
test = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
    , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
    , "<html><head><title><foo>lol<img /></foo></title></head><body><img /></body></html>"
    ]
