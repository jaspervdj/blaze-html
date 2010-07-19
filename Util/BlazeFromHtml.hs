-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module Main where

import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import qualified Data.Map as M

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
makeTree stack (TagPosition row _ : x : xs) = case x of
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
makeTree _ _ = error "TagSoup error"

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
fromHtml _ (Text text) = [show $ trim text]
  where
    -- Remove whitespace on both ends of a string
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
fromHtml _ (Comment comment) = map ("-- " ++) $ lines comment
fromHtml variant (Block block) = concatMap (fromHtml variant) block
fromHtml variant (Parent tag attrs inner) = case combinatorType variant tag of
    -- Actual parent tags
    ParentCombinator -> case inner of
        (Block _) -> (combinator ++ " $ do") : indent (fromHtml variant inner)
        -- We join non-block parents for better readability.
        x -> let ls = fromHtml variant x
                 apply = if dropApply x then " " else " $ "
             in case ls of (y : ys) -> (combinator ++ apply ++ y) : ys
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

    -- Check if we can drop the apply operator ($), for readability reasons.
    -- This would change:
    --
    -- > p $ "Some text"
    --
    -- Into
    --
    -- > p "Some text"
    --
    dropApply (Parent _ _ _) = False
    dropApply (Block _) = False
    dropApply _ = True

-- | Convert the HTML to blaze code.
--
blazeFromHtml :: HtmlVariant  -- ^ Variant to use
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> String       -- ^ Resulting code
blazeFromHtml variant name =
    addSignature . unlines . fromHtml variant
                 . minimizeBlocks . removeEmptyText . fst . makeTree []
                 . parseTagsOptions parseOptions { optTagPosition = True }
  where
    addSignature body =  name ++ " :: Html\n"
                      ++ name ++ " = " ++ body

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> help
        (x : xs) -> case M.lookup (x :: String) htmlVariants of
            Nothing      -> help
            Just variant -> main' variant xs
  where
    -- No files given, work with stdin
    main' variant [] = interact $ blazeFromHtml variant "template"

    -- Handle all files
    main' variant files = forM_ files $ \file -> do
        body <- readFile file
        putStrLn $ blazeFromHtml variant (dropExtension file) body

-- | Show some help information.
--
help :: IO ()
help = mapM_ putStrLn $
    [ "This is a tool to convert HTML code to BlazeHtml code. It is still"
    , "experimental and the results might need to be edited manually."
    , ""
    , "Usage:"
    , ""
    , "    blaze-from-html html-version [FILES ...]"
    , ""
    , "When no files are given, it works as a filter. The HTML version should"
    , "be one of:"
    , ""
    ] ++
    map (("- " ++) . fst) (M.toList htmlVariants) ++
    [ ""
    , "Example:"
    , ""
    , "    blaze-from-html html4-strict index.html"
    , ""
    ]
