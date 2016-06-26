-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module Main where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)
import Control.Arrow (first)
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import qualified Data.Map as M
import System.Console.GetOpt
import System.Exit
import System.IO

import Text.HTML.TagSoup

import Util.Sanitize (sanitize)
import Util.GenerateHtmlCombinators hiding (main)

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
                    deriving (Eq, Show)

-- | Traverse the list of tags to produce an intermediate representation of the
-- HTML tree.
--
makeTree :: HtmlVariant           -- ^ HTML variant used
         -> Bool                  -- ^ Should ignore errors
         -> [String]              -- ^ Stack of open tags
         -> [Tag String]          -- ^ Tags to parse
         -> (Html, [Tag String])  -- ^ (Result, unparsed part)
makeTree _ ignore stack []
    | null stack || ignore = (Block [], [])
    | otherwise = error $ "Error: tags left open at the end: " ++ show stack
makeTree variant ignore stack (TagPosition row _ : x : xs) = case x of
    TagOpen tag attrs -> if toLower' tag == "!doctype"
        then addHtml Doctype xs
        else let tag' = toLower' tag
                 (inner, t) = case combinatorType variant tag' of
                    LeafCombinator -> (Block [], xs)
                    _ -> makeTree variant ignore (tag' : stack) xs
                 p = Parent tag' (map (first toLower') attrs) inner
             in addHtml p t
    -- The closing tag must match the stack. If it is a closing leaf, we can
    -- ignore it
    TagClose tag ->
        let isLeafCombinator = combinatorType variant tag == LeafCombinator
            matchesStack = listToMaybe stack == Just (toLower' tag)
        in case (isLeafCombinator, matchesStack, ignore) of
            -- It's a leaf combinator, don't care about this element
            (True, _, _)          -> makeTree variant ignore stack xs
            -- It's a parent and the stack doesn't match
            (False, False, False) -> error $
                "Line " ++ show row ++ ": " ++ show tag ++ " closed but "
                        ++ show stack ++ " should be closed instead."
            -- Stack might not match but we ignore it anyway
            (False, _, _)         -> (Block [], xs)
    TagText text -> addHtml (Text text) xs
    TagComment comment -> addHtml (Comment comment) xs
    _ -> makeTree variant ignore stack xs
  where
    addHtml html xs' = let (Block l, r) = makeTree variant ignore stack xs'
                       in (Block (html : l), r)

    toLower' = map toLower
makeTree _ _ _ _ = error "TagSoup error"

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
    | combinator == "docTypeHtml" = ParentCombinator
    | combinator `elem` parents variant = ParentCombinator
    | combinator `elem` leafs variant = LeafCombinator
    | otherwise = UnknownCombinator

-- | Create a special @<html>@ parent that includes the docype.
--
joinHtmlDoctype :: Html -> Html
joinHtmlDoctype (Block (Doctype : Parent "html" attrs inner : xs)) =
    Block $ Parent "docTypeHtml" attrs inner : xs
joinHtmlDoctype x = x

-- | Produce the Blaze code from the HTML. The result is a list of lines.
--
fromHtml :: HtmlVariant  -- ^ Used HTML variant
         -> Options      -- ^ Building options
         -> Html         -- ^ HTML tree
         -> [String]     -- ^ Resulting lines of code
fromHtml _ _ Doctype = ["docType"]
fromHtml _ opts (Text text) = ["\"" ++ concatMap escape (trim text) ++ "\""]
  where
    -- Remove whitespace on both ends of a string
    trim
      | noTrimText_ opts = id
      | otherwise        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    -- Escape a number of characters
    escape '"'  = "\\\""
    escape '\n' = "\\n"
    escape '\\' = "\\\\"
    escape x    = [x]
fromHtml _ _ (Comment comment) = map ("-- " ++) $ lines comment
fromHtml variant opts (Block block) =
    concatMap (fromHtml variant opts) block
fromHtml variant opts (Parent tag attrs inner) =
    case combinatorType variant tag of
        -- Actual parent tags
        ParentCombinator -> case inner of
            (Block ls) -> if null ls
                then [combinator ++
                        (if null attrs then " " else " $ ") ++ "mempty"]
                else (combinator ++ " $ do") :
                        indent (fromHtml variant opts inner)
            -- We join non-block parents for better readability.
            x -> let ls = fromHtml variant opts x
                     apply = if dropApply x then " " else " $ "
                 in case ls of (y : ys) -> (combinator ++ apply ++ y) : ys
                               [] -> [combinator]

        -- Leaf tags
        LeafCombinator -> [combinator]

        -- Unknown tag
        UnknownCombinator -> if ignore_ opts
            then fromHtml variant opts inner
            else error $ "Tag " ++ tag ++ " is illegal in "
                                       ++ show variant
  where
    combinator = qualifiedSanitize "H." tag ++ attributes'
    attributes' = attrs >>= \(k, v) -> case k `elem` attributes variant of
        True  -> " ! " ++ qualifiedSanitize "A." k ++ " " ++ show v
        False -> case stripPrefix "data-" k of
            Just prefix -> " ! "
                        ++ "dataAttribute" ++ " "
                        ++ show prefix
                        ++ " " ++ show v
            Nothing | ignore_ opts -> ""
                    | otherwise  -> error $ "Attribute "
                                 ++ k ++ " is illegal in "
                                 ++ show variant

    -- Qualifies a tag with the given qualifier if needed, and sanitizes it.
    qualifiedSanitize qualifier tag' =
        (if isNameClash variant tag' then qualifier else "") ++ sanitize tag'

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
    dropApply _ = null attrs

-- | Produce the code needed for initial imports.
--
getImports :: HtmlVariant -> [String]
getImports variant =
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , import_ "Prelude"
    , qualify "Prelude" "P"
    , import_ "Data.Monoid (mempty)"
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
              -> Bool         -- ^ Produce standalone code
              -> Options      -- ^ Build options
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> String       -- ^ Resulting code
blazeFromHtml variant standalone opts name =
    unlines . addSignature . fromHtml variant opts
            . joinHtmlDoctype . minimizeBlocks
            . removeEmptyText . fst . makeTree variant (ignore_ opts) []
            . parseTagsOptions parseOptions { optTagPosition = True }
  where
    addSignature body = if standalone then [ name ++ " :: Html"
                                           , name ++ " = do"
                                           ] ++ indent body
                                      else body

-- | Indent block of code.
--
indent :: [String] -> [String]
indent = map ("    " ++)

-- | Main function
--
main :: IO ()
main = do
    args <- getOpt Permute options <$> getArgs
    let (o, n, errs) = args
    case () of
      _ | elem ArgHelp o  -> putStr help
        | not (null errs) -> do hPutStr stderr (concat errs)
                                hPutStrLn stderr "use -h for usage help"
                                exitFailure
        | otherwise       -> let v = getVariant o
                                 s = standalone' o
                                 i = ignore' o
                                 t = trim' o
                                 opts = Options i t
                             in do imports' v o
                                   main' v s opts n
  where
    -- No files given, work with stdin
    main' variant standalone opts [] = interact $
        blazeFromHtml variant standalone opts "template"

    -- Handle all files
    main' variant standalone opts files = forM_ files $ \file -> do
        body <- readFile file
        putStrLn $ blazeFromHtml variant standalone opts
                                 (dropExtension file) body

    -- Print imports if needed
    imports' variant opts = when (standalone' opts) $
        putStrLn $ unlines $ getImports variant

    -- Should we produce standalone code?
    standalone' opts = ArgStandalone `elem` opts

    -- Should we ignore errors?
    ignore' opts = ArgIgnoreErrors `elem` opts

    -- Should we trim whitespace from text?
    trim' opts = ArgNoTrimText `elem` opts

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
         | ArgStandalone
         | ArgIgnoreErrors
         | ArgNoTrimText
         | ArgHelp
         deriving (Show, Eq)

-- | The options record passed to 'blazeFromHtml'
--
data Options = Options
             { ignore_     :: Bool -- ^ ignore errors
             , noTrimText_ :: Bool -- ^ do not trim text
             }
  deriving (Show)

-- | A description of the options
--
options :: [OptDescr Arg]
options =
    [ Option "v" ["html-variant"] htmlVariantOption "HTML variant to use"
    , Option "s" ["standalone"] (NoArg ArgStandalone) "Produce standalone code"
    , Option "e" ["ignore-errors"] (NoArg ArgIgnoreErrors) "Ignore most errors"
    , Option "t" ["no-trim-text"]  (NoArg ArgNoTrimText) "Do not trim text"
    , Option "h" ["help"] (NoArg ArgHelp) "Show help"
    ]
  where
    htmlVariantOption = flip ReqArg "VARIANT" $ \name -> ArgHtmlVariant $
        fromMaybe (error $ "No HTML variant called " ++ name ++ " found.")
                  (M.lookup name htmlVariants)

-- | The default HTML variant
--
defaultHtmlVariant :: HtmlVariant
defaultHtmlVariant = html5
