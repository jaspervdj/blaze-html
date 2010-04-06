{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Builder.Latin1Builder
    ( Latin1Builder (..)
    , toLazyByteStringLatin1
    ) where

import Data.Monoid (Monoid, mconcat, mempty)
import Data.Char (ord)

import Data.Binary.Builder (Builder, singleton, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Internal.UnicodeSequence

-- | An efficient builder for lazy bytestrings representing Latin-1 (IS0
-- 8859-1) encoded sequences of Unicode characters -- non-representable
-- characters are dropped.
--
-- Use the functions from 'UnicodeSequence' and 'Monoid' to assemble such a
-- sequence. Convert it to a lazy bytestring using 'toLazyByteStringLatin1'.
newtype Latin1Builder = Latin1Builder
    { latin1Builder :: Builder
    } deriving (Monoid)

-- | Convert a sequence of Unicode characters represented as a
-- Latin1Builder to an ISO 8859-1 encoded sequence of bytes represented by a
-- lazy bytestring. Non-representable characters are dropped.
toLazyByteStringLatin1 :: Latin1Builder -> BL.ByteString
toLazyByteStringLatin1 = toLazyByteString . latin1Builder

instance UnicodeSequence Latin1Builder where
    -- FIXME: Provide more efficient implementation
    unicodeText   = mconcat . map unicodeChar . T.unpack
    unicodeChar c = case ord c of
        x | x <= 0xFF -> Latin1Builder $ singleton (fromIntegral x)
          | otherwise -> mempty
