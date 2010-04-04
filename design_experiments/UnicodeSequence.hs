{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An abstract representation of a sequence of Unicode characters with a
-- special focus on allowing for efficient transcoding.
--
-- NOTE that the same construction could also be used to abstract over
-- different representations (builders) for sequences of bytes. This way we
-- could have a unified interface to a byte sequence with and without
-- compression or whatever binary transformation needs to be done.
module UnicodeSequence where

import           Control.Exception          (assert)

import           Data.Bits                  (shiftR, (.&.))
import           Data.Binary.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Char                  (ord)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T  (decodeUtf8)
import qualified Data.Text.IO         as T  (putStrLn)
import           Data.Word

import           Debug.Trace                (trace)

-----------------------------------------------------------------------------
-- A type-class for abstracting sequences of unicode characters
-----------------------------------------------------------------------------


-- | A 'UnicodeSequence' is a type that can represent sequences built from
-- unicode characters represented as standard Haskell Char's and subsequences
-- of unicode characters represented as 'Text' values from 'Data.Text'.
--
-- NOTE that there is no instance 'UnicodeSequence s => UnicodeSequence [s]'
-- because then the possibly slow list mappend (++) would be used instead of
-- the (fast) mappend of the underlying implementation of the unicode sequence.
--
-- FIXME: It is somehow strange that we fix the possible representation of
-- unicode data here. A two level construction would be nicer. We'll leave
-- it for now because this way we have a better overview w.r.t. the
-- optimization GHC is doing.
class Monoid s => UnicodeSequence s where
    -- | A unicode character in the standard Haskell encoding represented by
    -- 'Char'.
    unicodeChar    :: Char -> s
    -- | A sequence of unicode characters represented as 'Text' from
    -- 'Data.Text'.
    unicodeText    :: Text -> s

instance UnicodeSequence s => UnicodeSequence (a -> s) where
    unicodeChar   = const . unicodeChar
    unicodeText   = const . unicodeText

-- | Build a unicode sequence from a string.
unicodeString :: UnicodeSequence s => String -> s
unicodeString s = (trace $ "string: "++s) (mconcat . map unicodeChar $ s)



-----------------------------------------------------------------------------
-- a few tests to see what if the sharing really works: it does :-)
-----------------------------------------------------------------------------

hello :: Utf8Builder
hello = unicodeString "hello"

world :: UnicodeSequence s => s
world = unicodeString "world"


displayBL = T.putStrLn . T.decodeUtf8 . mconcat . BL.toChunks

displayUtf8 = displayBL . toLazyByteStringUtf8 

testText1 :: Utf8Builder
testText1 = 
    mconcat $ 
        [ unicodeString "äeééé£££!!E+"
        , hello
        , unicodeChar ' '
        , world
        , unicodeString "äeééé£££!!E+"
        ]

testText2 :: Utf8Builder
testText2 = 
    mconcat $ 
        [ unicodeString "äeééé£££!!E+"
        , world
        , unicodeChar ' '
        , hello
        , unicodeString "äeééé£££!!E+"
        ]

------------------------------------------------------------------------------
-- A UTF-8 Unicode Sequence based on a ByteString Builder
------------------------------------------------------------------------------

-- | An efficient builder for lazy bytestrings representing UTF-8 encoded
-- sequences of Unicode characters. 
--
-- Use the functions from 'UnicodeSequence' and 'Monoid' to assemble such a
-- sequence. Convert it to a lazy bytestring using 'toLazyByteStringUtf8'.
newtype Utf8Builder = Utf8Builder { utf8Builder :: Builder }
    deriving( Monoid )

-- | Convert a sequence of Unicode characters represented as a Utf8Builder
-- to a UTF-8 encoded sequence of bytes represented by a lazy bytestring.
toLazyByteStringUtf8 :: Utf8Builder -> BL.ByteString
toLazyByteStringUtf8 = toLazyByteString . utf8Builder

instance UnicodeSequence Utf8Builder where
    unicodeChar   = Utf8Builder . encodeCharUtf8
    unicodeText   = Utf8Builder . encodeTextUtf8


-- Implementations of the encoding functions
--------------------------------------------

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

