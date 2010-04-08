{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Builder.Ascii7Builder
    ( Ascii7Builder (..)
    , toLazyByteStringAscii7
    ) where

import Data.Monoid (Monoid, mconcat, mempty)
import Data.Char (ord)

import Data.Binary.Builder (Builder, singleton, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Internal.Html

-- | An efficient builder for lazy bytestrings representing US-ASCII (7-bit)
-- encoded sequences of Unicode characters -- non-representable characters are
-- dropped.
--
-- Use the functions from 'UnicodeSequence' and 'Monoid' to assemble such a
-- sequence. Convert it to a lazy bytestring using 'toLazyByteStringAscii7'.
newtype Ascii7Builder = Ascii7Builder
    { ascii7Builder :: Builder
    } deriving( Monoid )

-- | Convert a sequence of Unicode characters represented as a Ascii7Builder to
-- an Ascii7 encoded sequence of bytes represented by a lazy bytestring.
-- Non-representable characters are dropped.
toLazyByteStringAscii7 :: Ascii7Builder -> BL.ByteString
toLazyByteStringAscii7 = toLazyByteString . ascii7Builder

instance UnicodeSequence Ascii7Builder where
    -- FIXME: Provide more efficient implementation
    unicodeText   = mconcat . map unicodeChar . T.unpack
    unicodeChar c = case ord c of
        x | x <= 0x7F -> Ascii7Builder $ singleton (fromIntegral x)
          | otherwise -> mempty
