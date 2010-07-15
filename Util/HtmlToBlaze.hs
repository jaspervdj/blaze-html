-- | A module for conversion from HTML to BlazeHtml Haskell code.
--
module HtmlToBlaze where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower, isSpace)

import Text.HTML.TagSoup

import Util.GenerateHtmlCombinators

-- | Traverse the list of tags to produce the Haskell code. The tags should be
-- parsed with position information, so we can produce proper error messages.
--
traverse :: HtmlVariant   -- ^ HTML variant used
         -> [String]      -- ^ Stack of open tags
         -> Int           -- ^ Indentation level
         -> [Tag String]  -- ^ Remaining tags to be parsed
         -> IO ()         -- ^ No result yet
traverse variant stack indent []
    | null stack = putStrLn "OK, done"
    | otherwise = putStrLn $ "Error: tags left open at the end: " ++ show stack
traverse variant stack indent (TagPosition row column : x : xs) = case x of
    -- Doctype is a special case!
    TagOpen tag attrs -> if map toLower tag == "!doctype"
        then do
            putStrLn $ "DOCTYPE"
            traverse variant stack indent xs
        else do
            -- TODO: Check for doctype, and leaf tags.
            putStrLn $ "Open " ++ tag
            -- Push the open tag on the stack and continue.
            traverse variant (tag : stack) (indent + 1) xs

    -- The closing tag must match the stack.
    TagClose tag -> if listToMaybe stack == Just tag
        -- If the stack is matched, we happily continue.
        then do
            putStrLn $ "Close " ++ tag
            traverse variant (tail stack) (indent - 1) xs
        -- Otherwise, we throw an error.
        else do
            error $  "Line " ++ show row ++ ": " ++ show tag ++ " closed but "
                  ++ show (listToMaybe stack) ++ " should be closed instead."

    -- We skip "empty" text nodes.
    TagText text -> do
        if all isSpace text
            then putStrLn $ "Empty text (skipping)"
            else putStrLn $ "Text " ++ text
        traverse variant stack indent xs

    TagComment comment -> do
        putStrLn $ "Comment " ++ comment
        traverse variant stack indent xs

    TagWarning str -> do
        putStrLn $ "Warning " ++ str
        traverse variant stack indent xs

    TagPosition _ _ -> do
        putStrLn $ "Position information"
        traverse variant stack indent xs

parse' :: String -> [Tag String]
parse' = parseTagsOptions parseOptions { optTagPosition = True }

test :: String
test = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
    , "    \"http://www.w3.org/TR/html4/frameset.dtd\">"
    , "<html><head><title>lol</title></head><body><img /></body></html>"
    ]
