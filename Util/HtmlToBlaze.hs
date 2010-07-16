-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module HtmlToBlaze where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)

import Text.HTML.TagSoup

import Util.Sanitize (sanitize)

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

-- | Produce the Blaze code from the HTML. The result is a list of lines.
--
fromHtml :: Html -> [String]
fromHtml Doctype = ["docType"]
fromHtml (Text text) = [text]
fromHtml (Comment comment) = ["-- " ++ comment]
fromHtml (Block block) = concatMap fromHtml block
fromHtml (Parent tag _ inner) = case inner of
    (Block b) -> (combinator ++ " $") : indent (fromHtml inner)
    -- We join non-block parents for better readability.
    x -> let ls = fromHtml x
         in case ls of (y : ys) -> (combinator ++ " $ " ++ y) : ys
                       [] -> [combinator]
  where
    indent :: [String] -> [String]
    indent = map ("    " ++)

    combinator = sanitize tag

-- | Convert the HTML to blaze code.
--
htmlToBlaze :: String -> String
htmlToBlaze = unlines . fromHtml
            . minimizeBlocks . removeEmptyText . fst . makeTree []
            . parseTagsOptions parseOptions { optTagPosition = True }

test :: String
test = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
    , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
    , "<html><head><title>lol</title></head><body><class /><img /></body></html>"
    ]
