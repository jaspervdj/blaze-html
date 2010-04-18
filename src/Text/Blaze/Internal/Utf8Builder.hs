module Text.Blaze.Internal.Utf8Builder 
    ( fromAscii7Char
    , fromSmallByteString
    , fromUnicodeShow
    , fromAscii7Show
    , fromHtmlString
    , fromHtmlText
    ) where

import Foreign
import Data.Char (ord)
import Prelude hiding (quot)

import Data.Binary.Builder (Builder, fromUnsafeWrite, singleton)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.Text (Text)
import qualified Data.Text as T

-- | /O(1)./ Convert a Haskell 'Char' into a builder, truncating it.
--
fromAscii7Char :: Char -> Builder
fromAscii7Char = singleton . fromIntegral . ord
{-# INLINE fromAscii7Char #-}

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it. This is a well
-- suited function for strings consisting only of Ascii7 characters. This
-- function should perform better when dealing with small strings than the
-- fromByteString function from Builder.
--
fromSmallByteString :: S.ByteString -> Builder
fromSmallByteString byteString = fromUnsafeWrite l f
  where
    (fptr, o, l) = S.toForeignPtr byteString
    f dst = do copyBytes dst (unsafeForeignPtrToPtr fptr `plusPtr` o) l
               touchForeignPtr fptr
    {-# INLINE f #-}

-- | /O(n)./ Convert a showable datatype to a builder. Use this function when
-- the result of 'show' will contain Unicode characters.
fromUnicodeShow :: Show a => a -> Builder
fromUnicodeShow s =
    let (l, f) = foldl writeUnicodeChar (0, const $ return ()) (show s)
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a showable datatype to a builder. Use this function when
-- the result of 'show' will not contain Unicode characters.
fromAscii7Show :: Show a => a -> Builder
fromAscii7Show s =
    let (l, f) = foldl writeAscii7Char (0, const $ return ()) (show s)
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a showable datatype to a builder. Use this function when
-- the result of 'show' will not contain Unicode characters.
fromHtmlString :: String -> Builder
fromHtmlString s =
    let (l, f) = foldl writeHtmlUnicodeChar (0, const $ return ()) s
    in fromUnsafeWrite l f

-- | /O(n)./ Convert a 'Text' value to a Builder, doing HTML escaping as well.
fromHtmlText :: Text -> Builder
fromHtmlText text =
    let (l, f) = T.foldl writeHtmlUnicodeChar (0, const $ return ()) text
    in fromUnsafeWrite l f

writeUnicodeChar :: (Int, Ptr Word8 -> IO ())
                 -> Char
                 -> (Int, Ptr Word8 -> IO ())
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

writeAscii7Char :: (Int, Ptr Word8 -> IO ())
                 -> Char
                 -> (Int, Ptr Word8 -> IO ())
writeAscii7Char (l, f) c =
    let x = ord c
    in l `seq` (l + 1, \ptr -> f ptr >> poke (ptr `plusPtr` l)
                                             (fromIntegral x :: Word8))
{-# INLINE writeAscii7Char #-}

writeHtmlUnicodeChar :: (Int, Ptr Word8 -> IO ())
                     -> Char
                     -> (Int, Ptr Word8 -> IO ())
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
