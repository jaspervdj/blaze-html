{-# LANGUAGE OverloadedStrings #-}
-- | Module extending 'Utf8Builder' with HTML-specific functions.
--
module Text.Blaze.Internal.Utf8BuilderHtml
    ( 
      -- * Creating builders with HTML escaping.
      escapeHtmlFromChar
    , escapeHtmlFromText
    , escapeHtmlFromString
    ) where

import Data.Monoid (Monoid (..))

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Internal.Utf8Builder

-- | /O(1)./ Convert a Haskell character to a 'Utf8Builder'.
--
escapeHtmlFromChar :: Char -> Utf8Builder
escapeHtmlFromChar = fromUnsafeWrite . escapeHtmlWriteChar

-- | /O(n)./ Convert a 'Text' value to a 'Utf8Builder'. This function does
-- proper HTML escaping.
--
escapeHtmlFromText :: Text -> Utf8Builder
escapeHtmlFromText text = fromUnsafeWrite $
    T.foldl (\w c -> w `mappend` escapeHtmlWriteChar c) mempty text
--
--SM: The above construction is going to kill you (in terms of memory and
--latency) if the text is too long.  Could you ensure that the text is written
--chunkwise? Perhaps, by first breaking the Builder abstraction again and
--inlining the check for enough free memory into the loop. The basic check
--should be equally expensive as summing up the length.
--

-- | /O(n)./ Convert a Haskell 'String' to a 'Utf8Builder'. This function does
-- proper escaping for HTML entities.
--
escapeHtmlFromString :: String -> Utf8Builder
escapeHtmlFromString = writeList escapeHtmlWriteChar
 -- fromUnsafeWrite $ 
  --  foldl (\w c -> w `mappend` escapeHtmlWriteChar c) mempty string

-- | Write an unicode character to a 'Builder', doing HTML escaping.
--
escapeHtmlWriteChar :: Char   -- ^ Character to write.
                    -> Write  -- ^ Resulting write.
escapeHtmlWriteChar '<' = optimizeWriteBuilder $ fromText "&lt;"
escapeHtmlWriteChar '>' = optimizeWriteBuilder $ fromText "&gt;"
escapeHtmlWriteChar '&' = optimizeWriteBuilder $ fromText "&amp;"
escapeHtmlWriteChar '"' = optimizeWriteBuilder $ fromText "&quot;"
escapeHtmlWriteChar '\'' = optimizeWriteBuilder $ fromText "&apos;"
escapeHtmlWriteChar c = writeChar c
{-# INLINE escapeHtmlWriteChar #-}
