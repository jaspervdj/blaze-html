{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Builder.Utf8Builder
    ( Utf8Builder (..)
    , toLazyByteStringUtf8
    ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Monoid (Monoid (..))

import Data.Binary.Builder (Builder, singleton, toLazyByteString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Internal.UnicodeSequence

-- | An efficient builder for lazy bytestrings representing UTF-8 encoded
-- sequences of Unicode characters. 
--
-- Use the functions from @UnicodeSequence@ and @Monoid@ to assemble such a
-- sequence. Convert it to a lazy bytestring using @toLazyByteStringUtf8@.
newtype Utf8Builder = Utf8Builder
    { -- | The actual builder used.
      utf8Builder :: Builder
    } deriving (Monoid)

-- | Convert a sequence of Unicode characters represented as a Utf8Builder
-- to a UTF-8 encoded sequence of bytes represented by a lazy bytestring.
toLazyByteStringUtf8 :: Utf8Builder -> BL.ByteString
toLazyByteStringUtf8 = toLazyByteString . utf8Builder

instance UnicodeSequence Utf8Builder where
    unicodeChar   = Utf8Builder . encodeCharUtf8
    unicodeText   = Utf8Builder . encodeTextUtf8

-- based on: encode_utf8 in GHC.IO.Encoding.UTF8
encodeCharUtf8 :: Char -> Builder
encodeCharUtf8 c = 
    case ord c of
      x | x <= 0x7F   ->
             singleton $ fromIntegral x
        | x <= 0x07FF ->
             let 
                x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
                x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
             in
                singleton x1 `mappend`
                singleton x2
        | x <= 0xFFFF -> 
             let 
                x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
                x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                x3 = fromIntegral $ (x .&. 0x3F) + 0x80
             in
                singleton x1 `mappend`
                singleton x2 `mappend`
                singleton x3
        | otherwise ->
             let 
                x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
                x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
                x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                x4 = fromIntegral $ (x .&. 0x3F) + 0x80
             in
                singleton x1 `mappend`
                singleton x2 `mappend`
                singleton x3 `mappend`
                singleton x4

-- FIXME: Use a more efficient implementation based on 'restreamUtf8' from
-- 'Data.Text.Encoding.Fusion.Common'.
encodeTextUtf8 :: Text -> Builder
encodeTextUtf8 = mconcat . map encodeCharUtf8 . T.unpack
