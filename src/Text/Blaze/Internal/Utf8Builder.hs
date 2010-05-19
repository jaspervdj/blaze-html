-- | A module for efficiently constructing a 'Builder'. This module offers more
-- functions than the standard ones, and more HTML-specific functions.
--
module Text.Blaze.Internal.Utf8Builder 
    ( 
      -- * Creating Builders from Text.
      fromText
    , fromEscapedText

      -- * Creating Builders from ByteStrings.
    , fromEscapedByteString

      -- * Creating Builders from characters.
    , fromEscapedAscii7Char

      -- * Creating Builders from Strings.
    , fromString
    , fromEscapedString
    ) where

import Foreign
import Data.Char (ord)
import Prelude hiding (quot)

import Data.Binary.Builder (Builder, fromUnsafeWrite, singleton)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.Text (Text)
import qualified Data.Text as T

-- | /O(n)./ Convert a 'Text' value to a 'Builder'. This function does proper
-- HTML escaping.
--
fromText :: Text -> Builder
fromText text =
    let (l, f) = T.foldl writeUnicodeChar writeNothing text
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a 'Text' value to a 'Builder'. This function will not do
-- any HTML escaping.
--
fromEscapedText :: Text -> Builder
fromEscapedText text =
    let (l, f) = T.foldl writeEscapedUnicodeChar writeNothing text
    in fromUnsafeWrite l f

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it. This is a well
-- suited function for strings consisting only of Ascii7 characters. This
-- function should perform better when dealing with small strings than the
-- fromByteString function from Builder.
--
fromEscapedByteString :: S.ByteString -> Builder
fromEscapedByteString byteString = fromUnsafeWrite l f
  where
    (fptr, o, l) = S.toForeignPtr byteString
    f dst = do copyBytes dst (unsafeForeignPtrToPtr fptr `plusPtr` o) l
               touchForeignPtr fptr
    {-# INLINE f #-}

-- | /O(1)./ Convert a Haskell character to a 'Builder', truncating it to a
-- byte, and not doing any escaping.
--
fromEscapedAscii7Char :: Char -> Builder
fromEscapedAscii7Char = singleton . fromIntegral . ord
{-# INLINE fromEscapedAscii7Char #-}

-- | /O(n)./ Convert a Haskell 'String' to a 'Builder'. This function does
-- proper escaping for HTML entities.
--
fromString :: String -> Builder
fromString s =
    let (l, f) = foldl writeUnicodeChar writeNothing s
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a Haskell 'String' to a builder. Unlike 'fromHtmlString',
-- this function will not do any escaping.
--
fromEscapedString :: String -> Builder
fromEscapedString s =
    let (l, f) = foldl writeEscapedUnicodeChar writeNothing s
    in fromUnsafeWrite l f

-- | Function to create an empty write. This is used as initial value for folds.
--
writeNothing :: (Int, Ptr Word8 -> IO ())
writeNothing = (0, const $ return ())
{-# INLINE writeNothing #-}

-- | Write an unicode character to a 'Builder', doing HTML escaping.
--
writeUnicodeChar :: (Int, Ptr Word8 -> IO ()) -- ^ Current write state.
                 -> Char                      -- ^ Character to write.
                 -> (Int, Ptr Word8 -> IO ()) -- ^ Resulting state.
writeUnicodeChar (l, f) '<' =
    (l + 4, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) lt)
  where
    lt :: [Word8]
    lt = map (fromIntegral . ord) "&lt;"
writeUnicodeChar (l, f) '>' =
    (l + 4, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) gt)
  where
    gt :: [Word8]
    gt = map (fromIntegral . ord) "&gt;"
writeUnicodeChar (l, f) '&' =
    (l + 5, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) amp)
  where
    amp :: [Word8]
    amp = map (fromIntegral . ord) "&amp;"
writeUnicodeChar (l, f) '"' =
    (l + 6, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) quot)
  where
    quot :: [Word8]
    quot = map (fromIntegral . ord) "&quot;"
writeUnicodeChar (l, f) '\'' =
    (l + 6, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) apos)
  where
    apos :: [Word8]
    apos = map (fromIntegral . ord) "&apos;"
writeUnicodeChar (l, f) c = writeEscapedUnicodeChar (l, f) c
{-# INLINE writeUnicodeChar #-}

-- | Write a Unicode character, encoding it as UTF-8.
--
writeEscapedUnicodeChar :: (Int, Ptr Word8 -> IO ())  -- ^ Current state.
                        -> Char                       -- ^ Character to write.
                        -> (Int, Ptr Word8 -> IO ())  -- ^ Resulting state.
writeEscapedUnicodeChar (l, f) c = l `seq` encodeCharUtf8 f1 f2 f3 f4 c
  where
    f1 x = (l + 1, \ptr -> f ptr >> poke (ptr `plusPtr` l) x)

    f2 x1 x2 = (l + 2, \ptr -> let pos = ptr `plusPtr` l
                               in f ptr >> poke pos x1
                                        >> poke (pos `plusPtr` 1) x2)

    f3 x1 x2 x3 = (l + 3, \ptr -> let pos = ptr `plusPtr` l
                                  in f ptr >> poke pos x1
                                           >> poke (pos `plusPtr` 1) x2
                                           >> poke (pos `plusPtr` 2) x3)

    f4 x1 x2 x3 x4 = (l + 4, \ptr -> let pos = ptr `plusPtr` l
                                     in f ptr >> poke pos x1
                                              >> poke (pos `plusPtr` 1) x2
                                              >> poke (pos `plusPtr` 2) x3
                                              >> poke (pos `plusPtr` 3) x4)
{-# INLINE writeEscapedUnicodeChar #-}

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8.
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8.
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8.
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8.
               -> Char                                     -- ^ Input 'Char'.
               -> a                                        -- ^ Result.
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0xFF -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}
