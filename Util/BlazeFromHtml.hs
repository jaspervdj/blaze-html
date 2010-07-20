-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module Main where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import qualified Data.Map as M
import System.Console.GetOpt

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
fromHtml _ (Text text) = ["\"" ++ escapeQuotes (trim text) ++ "\""]
  where
    -- Remove whitespace on both ends of a string
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    -- Escape double quotes
    escapeQuotes = concatMap (\x -> if x == '"' then "\\\"" else [x])
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

-- | Produce the code needed for initial imports.
--
getImports :: HtmlVariant -> [String]
getImports variant =
    [ import_ "Prelude"
    , qualify "Prelude" "P"
    , ""
    , import_ h
    , qualify h "H"
    , import_ a
    , qualify a "A"
    ]
  where
    import_ = ("import " ++)
    qualify name short = "import qualified " ++ name ++ " as " ++ short
    h = getModuleName variant
    a = getAttributeModuleName variant

-- | Convert the HTML to blaze code.
--
blazeFromHtml :: HtmlVariant  -- ^ Variant to use
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> String       -- ^ Resulting code
blazeFromHtml variant name =
    unlines . addSignature . fromHtml variant
            . minimizeBlocks . removeEmptyText . fst . makeTree []
            . parseTagsOptions parseOptions { optTagPosition = True }
  where
    addSignature body = [ name ++ " :: Html"
                        , name ++ " = do"
                        ] ++ indent body

-- | Indent block of code.
--
indent :: [String] -> [String]
indent = map ("    " ++)

-- | Main function
--
main :: IO ()
main = do
    args <- getOpt Permute options <$> getArgs
    case args of
        (o, n, []) -> let v = getVariant o
                      in imports' v o >> main' v n
        (_, _, _)  -> putStr help
  where
    -- No files given, work with stdin
    main' variant [] = interact $ blazeFromHtml variant "template"

    -- Handle all files
    main' variant files = forM_ files $ \file -> do
        body <- readFile file
        putStrLn $ blazeFromHtml variant (dropExtension file) body

    -- Print imports if needed
    imports' variant opts = when (ArgAddImports `elem` opts) $
        putStrLn $ unlines $ getImports variant

    -- Get the variant from the options
    getVariant opts = fromMaybe defaultHtmlVariant $ listToMaybe $
        flip concatMap opts $ \o -> case o of (ArgHtmlVariant x) -> [x]
                                              _ -> []

-- | Help information.
--
help :: String
help = unlines $
    [ "This is a tool to convert HTML code to BlazeHtml code. It is still"
    , "experimental and the results might need to be edited manually."
    , ""
    , "USAGE"
    , ""
    , "  blaze-from-html [OPTIONS...] [FILES ...]"
    , ""
    , "When no files are given, it works as a filter."
    , ""
    , "EXAMPLE"
    , ""
    , "  blaze-from-html -v html4-strict index.html"
    , ""
    , "This converts the index.html file to Haskell code, writing to stdout."
    , ""
    , "OPTIONS"
    , usageInfo "" options
    , "VARIANTS"
    , ""
    ] ++
    map (("  " ++) . fst) (M.toList htmlVariants) ++
    [ ""
    , "By default, " ++ show defaultHtmlVariant ++ " is used."
    ]

-- | Options for the CLI program
--
data Arg = ArgHtmlVariant HtmlVariant
         | ArgAddImports
         deriving (Show, Eq)

-- | A description of the options
--
options :: [OptDescr Arg]
options =
    [ Option "v" ["html-variant"] htmlVariantOption "HTML variant to use"
    , Option "i" ["imports"] (NoArg ArgAddImports) "Add initial imports"
    ]
  where
    htmlVariantOption = flip ReqArg "VARIANT" $ \name -> ArgHtmlVariant $
        fromMaybe (error $ "No HTML variant called " ++ name ++ " found.")
                  (M.lookup name htmlVariants)

-- | The default HTML variant
--
defaultHtmlVariant :: HtmlVariant
defaultHtmlVariant = html5
