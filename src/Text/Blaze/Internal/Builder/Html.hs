{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Internal.Builder.Html
    ( writeHtmlEscapedChar
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    ) where

import Data.ByteString.Char8 ()

import Text.Blaze.Internal.Builder.Core
import Text.Blaze.Internal.Builder.Utf8

-- | Write an unicode character to a 'Builder', doing HTML escaping.
--
writeHtmlEscapedChar :: Char   -- ^ Character to write.
                     -> Write  -- ^ Resulting write.
writeHtmlEscapedChar '<'  = writeByteString "&lt;"
writeHtmlEscapedChar '>'  = writeByteString "&gt;"
writeHtmlEscapedChar '&'  = writeByteString "&amp;"
writeHtmlEscapedChar '"'  = writeByteString "&quot;"
writeHtmlEscapedChar '\'' = writeByteString "&apos;"
writeHtmlEscapedChar c    = writeChar c
{-# INLINE writeHtmlEscapedChar #-}

-- | A HTML escaped 'Char'.
--
fromHtmlEscapedChar :: Char     -- ^ Character to write.
                    -> Builder  -- ^ Resulting 'Builder'.
fromHtmlEscapedChar = writeSingleton writeHtmlEscapedChar

-- | A HTML escaped 'String'.
--
fromHtmlEscapedString :: String   -- ^ String to create a 'Builder' from.
                      -> Builder  -- ^ Resulting 'Builder'.
fromHtmlEscapedString = writeList writeHtmlEscapedChar
