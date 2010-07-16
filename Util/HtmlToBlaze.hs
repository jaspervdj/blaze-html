-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module HtmlToBlaze where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)

import Text.HTML.TagSoup

import Util.GenerateHtmlCombinators

-- | Simple type to represent attributes.
--
type Attributes = [(String, String)]

-- | Intermediate tree representation. This representation contains several
-- constructors aimed at pretty-printing.
--
data Html = Parent String Attributes Html
          | Block [Html]
          | Text String
          | Comment String
          | Doctype
          deriving (Show)

-- | Traverse the list of tags to produce an intermediate representation of the
-- HTML tree.
--
makeTree :: [String]                -- ^ Stack of open tags
         -> [Tag String]            -- ^ Tags to parse
         -> (Html, [Tag String])  -- ^ (Result, unparsed part)
makeTree stack []
    | null stack = (Block [], [])
    | otherwise = error $ "Error: tags left open at the end: " ++ show stack
makeTree stack (TagPosition row column : x : xs) = case x of
    TagOpen tag attrs -> if toLower' tag == "!doctype"
        then addHtml Doctype xs
        else let (inner, t) = makeTree (tag : stack) xs
                 p = Parent (toLower' tag) (map (first toLower') attrs) inner
             in addHtml p t
    -- The closing tag must match the stack.
    TagClose tag -> if listToMaybe stack == Just (toLower' tag)
        then (Block [], xs)
        else error $  "Line " ++ show row ++ ": " ++ show tag ++ " closed but "
                   ++ show stack ++ " should be closed instead."
    TagText text -> addHtml (Text text) xs
    TagComment comment -> addHtml (Comment comment) xs
    _ -> makeTree stack xs
  where
    addHtml html xs' = let (Block l, r) = makeTree stack xs'
                       in (Block (html : l), r)

    toLower' = map toLower

-- | Remove empty text from the HTML.
--
removeEmptyText :: Html -> Html
removeEmptyText (Block b) = Block $ map removeEmptyText $ flip filter b $ \h ->
    case h of Text text -> any (not . isSpace) text
              _         -> True
removeEmptyText (Parent tag attrs inner) =
    Parent tag attrs $ removeEmptyText inner
removeEmptyText x = x

-- | Try to eliminiate Block constructors as much as possible.
--
minimizeBlocks :: Html -> Html
minimizeBlocks (Parent t a (Block [x])) = minimizeBlocks $ Parent t a x
minimizeBlocks (Parent t a x) = Parent t a $ minimizeBlocks x
minimizeBlocks (Block x) = Block $ map minimizeBlocks x
minimizeBlocks x = x

-- | Convert the HTML to blaze code.
--
{-
toBlaze :: Int -> [Html] -> String
toBlaze indent [] = ""
toBlaze indent (x : xs) = case x of
    Text text -> text ++ continue
    Comment comment -> "-- " ++ comment ++ continue
    Doctype -> "doctype" ++ continue
    Parent tag _ inner -> tag ++ case inner of
        [_] -> " $ " ++ toBlaze indent inner ++ toBlaze indent xs
        _ -> " $\n" ++ ind (indent + 1) ++ toBlaze (indent + 1) inner
                    ++ toBlaze indent xs
  where
    ind i = replicate (i * 4) ' '
    continue = "\n" ++ case xs of
        (_ : _) -> ind indent ++ toBlaze indent xs
        [] -> ind (indent - 1)
-}

htmlToBlaze = minimizeBlocks . removeEmptyText . fst . makeTree []
            . parseTagsOptions parseOptions { optTagPosition = True }

parse' :: String -> [Tag String]
parse' = parseTagsOptions parseOptions { optTagPosition = True }

test :: String
test = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
    , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
    , "<html><head><title><foo>lol<img /></foo></title></head><body><img /></body></html>"
    ]
