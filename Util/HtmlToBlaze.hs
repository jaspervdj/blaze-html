-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module HtmlToBlaze where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)

import Text.HTML.TagSoup

import Util.Sanitize (sanitize)
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

-- | Different combinator types.
--
data CombinatorType = ParentCombinator
                    | LeafCombinator
                    | UnknownCombinator
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

-- | Get the type of a combinator, using a given variant.
--
combinatorType :: HtmlVariant -> String -> CombinatorType
combinatorType variant combinator
    | combinator `elem` parents variant = ParentCombinator
    | combinator `elem` leafs variant = LeafCombinator
    | otherwise = UnknownCombinator

-- | Produce the Blaze code from the HTML. The result is a list of lines.
--
fromHtml :: HtmlVariant -> Html -> [String]
fromHtml _ Doctype = ["docType"]
fromHtml _ (Text text) = [show text]
fromHtml _ (Comment comment) = ["-- " ++ comment]
fromHtml variant (Block block) = concatMap (fromHtml variant) block
fromHtml variant (Parent tag attrs inner) = case combinatorType variant tag of
    -- Actual parent tags
    ParentCombinator -> case inner of
        (Block b) -> (combinator ++ " $ do") : indent (fromHtml variant inner)
        -- We join non-block parents for better readability.
        x -> let ls = fromHtml variant x
             in case ls of (y : ys) -> (combinator ++ " $ " ++ y) : ys
                           [] -> [combinator]
    -- Leaf tags
    LeafCombinator -> [combinator]

    -- Unknown tag
    UnknownCombinator -> error $ "Unknown tag: " ++ tag
  where
    indent = map ("    " ++)
    combinator = sanitize tag ++ attributes'
    attributes' = attrs >>= \(k, v) -> if k `elem` attributes variant
        then " ! " ++ sanitize k ++ " " ++ show v
        else error $ "Unknown attribute: " ++ k

-- | Convert the HTML to blaze code.
--
htmlToBlaze :: HtmlVariant -> String -> String
htmlToBlaze variant = unlines . fromHtml variant
                    . minimizeBlocks . removeEmptyText . fst . makeTree []
                    . parseTagsOptions parseOptions { optTagPosition = True }

test :: String
test = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
    , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
    , "<html><head><title>lol</title></head><body style=\"lol\">"
    , "Haha<img src=\"foo.png\" alt=\"bar\"/></body></html>"
    ]
