-- | A module for efficiently constructing a 'Builder'. This module offers more
-- functions than the standard ones, and more HTML-specific functions.
--
module Text.Blaze.Internal.Utf8Builder 
    ( 
      -- * Creating Builders from Text.
      fromHtmlText
    , fromRawText

      -- * Creating Builders from Showable values.
    , fromHtmlShow
    , fromRawShow

      -- * Creating Builders from ByteStrings.
    , fromRawByteString

      -- * Creating Builders from characters.
    , fromRawAscii7Char

      -- * Creating Builders from Strings.
    , fromHtmlString
    , fromRawString
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
fromHtmlText :: Text -> Builder
fromHtmlText text =
    let (l, f) = T.foldl writeHtmlUnicodeChar writeNothing text
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a 'Text' value to a 'Builder'. This function will not do
-- any HTML escaping.
--
fromRawText :: Text -> Builder
fromRawText text =
    let (l, f) = T.foldl writeUnicodeChar writeNothing text
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a 'Show'able datatype to a 'Builder', doing HTML escaping.
--
fromHtmlShow :: Show a => a -> Builder
fromHtmlShow = fromHtmlString . show
{-# INLINE fromHtmlShow #-}

-- | /O(n)./ Convert a 'Show'able datatype to a 'Builder', not doing any
-- escaping.
--
fromRawShow :: Show a => a -> Builder
fromRawShow = fromRawString . show
{-# INLINE fromRawShow #-}

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it. This is a well
-- suited function for strings consisting only of Ascii7 characters. This
-- function should perform better when dealing with small strings than the
-- fromByteString function from Builder.
--
fromRawByteString :: S.ByteString -> Builder
fromRawByteString byteString = fromUnsafeWrite l f
  where
    (fptr, o, l) = S.toForeignPtr byteString
    f dst = do copyBytes dst (unsafeForeignPtrToPtr fptr `plusPtr` o) l
               touchForeignPtr fptr
    {-# INLINE f #-}

-- | /O(1)./ Convert a Haskell character to a 'Builder', truncating it to a
-- byte, and not doing any escaping.
--
fromRawAscii7Char :: Char -> Builder
fromRawAscii7Char = singleton . fromIntegral . ord
{-# INLINE fromRawAscii7Char #-}

-- | /O(n)./ Convert a Haskell 'String' to a 'Builder'. This function does
-- proper escaping for HTML entities.
--
fromHtmlString :: String -> Builder
fromHtmlString s =
    let (l, f) = foldl writeHtmlUnicodeChar writeNothing s
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a Haskell 'String' to a builder. Unlike 'fromHtmlString',
-- this function will not do any escaping.
--
fromRawString :: String -> Builder
fromRawString s =
    let (l, f) = foldl writeUnicodeChar writeNothing s
    in fromUnsafeWrite l f

-- | Function to create an empty write. This is used as initial value for folds.
writeNothing :: (Int, Ptr Word8 -> IO ())
writeNothing = (0, const $ return ())
{-# INLINE writeNothing #-}

-- | Write a Unicode character, encoding it as UTF-8.
writeUnicodeChar :: (Int, Ptr Word8 -> IO ()) -- ^ Current state.
                 -> Char                      -- ^ Character to write.
                 -> (Int, Ptr Word8 -> IO ()) -- ^ Resulting state.
writeUnicodeChar (l, f) c = l `seq` case ord c of
    x | x <= 0xFF -> (l + 1, \ptr -> f ptr >> poke (ptr `plusPtr` l)
                                                   (fromIntegral x :: Word8))
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in (l + 2, \ptr ->
               let pos = ptr `plusPtr` l
               in f ptr >> poke pos (x1 :: Word8)
                        >> poke (pos `plusPtr` 1) (x2 :: Word8))
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in (l + 3, \ptr ->
               let pos = ptr `plusPtr` l
               in f ptr >> poke pos (x1 :: Word8)
                        >> poke (pos `plusPtr` 1) (x2 :: Word8)
                        >> poke (pos `plusPtr` 2) (x3 :: Word8))
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in (l + 4, \ptr ->
               let pos = ptr `plusPtr` l
               in f ptr >> poke pos (x1 :: Word8)
                        >> poke (pos `plusPtr` 1) (x2 :: Word8)
                        >> poke (pos `plusPtr` 2) (x3 :: Word8)
                        >> poke (pos `plusPtr` 3) (x4 :: Word8))
{-# INLINE writeUnicodeChar #-}

-- | Write an unicode character to a 'Builder', doing HTML escaping.
writeHtmlUnicodeChar :: (Int, Ptr Word8 -> IO ()) -- ^ Current write state.
                     -> Char                      -- ^ Character to write.
                     -> (Int, Ptr Word8 -> IO ()) -- ^ Resulting state.
writeHtmlUnicodeChar (l, f) '<' =
    (l + 4, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) lt)
  where
    lt :: [Word8]
    lt = map (fromIntegral . ord) "&lt;"
writeHtmlUnicodeChar (l, f) '>' =
    (l + 4, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) gt)
  where
    gt :: [Word8]
    gt = map (fromIntegral . ord) "&gt;"
writeHtmlUnicodeChar (l, f) '&' =
    (l + 5, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) amp)
  where
    amp :: [Word8]
    amp = map (fromIntegral . ord) "&amp;"
writeHtmlUnicodeChar (l, f) '"' =
    (l + 6, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) quot)
  where
    quot :: [Word8]
    quot = map (fromIntegral . ord) "&quot;"
writeHtmlUnicodeChar (l, f) '\'' =
    (l + 6, \ptr -> f ptr >> pokeArray (ptr `plusPtr` l) apos)
  where
    apos :: [Word8]
    apos = map (fromIntegral . ord) "&apos;"
writeHtmlUnicodeChar (l, f) c = writeUnicodeChar (l, f) c
{-# INLINE writeHtmlUnicodeChar #-}
