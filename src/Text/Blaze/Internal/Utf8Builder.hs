{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module for efficiently constructing a 'Builder'. This module offers more
-- functions than the standard ones, and more HTML-specific functions.
--
--
--  SM: General remark: Try to split it into Utf8 specific parts and a
--  HtmlBuilder using it. Essentially, a UTF-8 builder is a Text builder that
--  uses UTF-8 for its internal representation. The Text builder from Tom
--  Harper would then be called Utf16Builder. They should offer exactly the
--  same interface (except perhaps for the extraction functions.)
--
module Text.Blaze.Internal.Utf8Builder 
    ( 
      -- * The Utf8Builder type.
      Utf8Builder

      -- * Creating Builders from Text.
    , fromText
    , fromPreEscapedText
--SM: It seems strange to me that a Utf8 builder cares about escaping. This
--should be part of a HtmlBuilder if at all.
--
--The invariant added by a Utf8Builder is that it is a UTF-8 encoded sequence
--of Unicode characters.

      -- * Creating Builders from ByteStrings.
    , unsafeFromByteString

      -- * Creating Builders from characters.
    , fromPreEscapedAscii7Char
--SM: I'm not sure if this is needed. There are few places where the caller can
--    guarantee that his char is really Ascii7. I would incorporate these places
--    like showing ints or the like into the API of an Utf8Builder if it is really
--    needed for speed.
--
--    Or otherwise I would offer the interface of fromUnsafeBytestring and
--    fromUnsafeByte.

      -- * Creating Builders from Strings.
    , fromString
    , fromPreEscapedString

      -- * Extracting the value from the builder.
    , toLazyByteString
--SM: Here, I would expect that I get two functions
--
-- toText           :: Utf8Builder -> Text
-- toLazyByteString :: Utf8Builder -> LB.ByteString
    ) where

import Foreign
import Data.Char (ord)
import Data.Monoid (Monoid)
import Prelude hiding (quot)

import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T

-- | A newtype definition for the UTF-8 builder monoid.
newtype Utf8Builder = Utf8Builder Builder
    deriving (Monoid)

-- | /O(n)./ Convert a 'Text' value to a 'Utf8Builder'. This function does
-- proper HTML escaping.
--
fromText :: Text -> Utf8Builder
fromText text =
    let (l, f) = T.foldl writeUnicodeChar writeNothing text
    in Utf8Builder $ B.fromUnsafeWrite l f
--
--SM: The above construction is going to kill you (in terms of memory and
--latency) if the text is too long.  Could you ensure that the text is written
--chunkwise? Perhaps, by first breaking the Builder abstraction again and
--inlining the check for enough free memory into the loop. The basic check
--should be equally expensive as summing up the length.
--

-- | /O(n)./ Convert a 'Text' value to a 'Utf8Builder'. This function will not
-- do any HTML escaping.
--
fromPreEscapedText :: Text -> Utf8Builder
fromPreEscapedText text =
    let (l, f) = T.foldl writePreEscapedUnicodeChar writeNothing text
    in Utf8Builder $ B.fromUnsafeWrite l f

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it. This function is
-- considered unsafe, as a `S.ByteString` can contain invalid UTF-8 bytes, so
-- you chould use it with caution. This function should perform better when
-- dealing with small strings than the fromByteString function from Builder.
--
unsafeFromByteString :: S.ByteString -> Utf8Builder
unsafeFromByteString byteString = Utf8Builder $ B.fromUnsafeWrite l f
  where
    (fptr, o, l) = S.toForeignPtr byteString
    f dst = do copyBytes dst (unsafeForeignPtrToPtr fptr `plusPtr` o) l
               touchForeignPtr fptr
    {-# INLINE f #-}

-- | /O(1)./ Convert a Haskell character to a 'Utf8Builder', truncating it to a
-- byte, and not doing any escaping.
--
fromPreEscapedAscii7Char :: Char -> Utf8Builder
fromPreEscapedAscii7Char = Utf8Builder . B.singleton . fromIntegral . ord
{-# INLINE fromPreEscapedAscii7Char #-}

-- | /O(n)./ Convert a Haskell 'String' to a 'Utf8Builder'. This function does
-- proper escaping for HTML entities.
--
fromString :: String -> Utf8Builder
fromString s =
    let (l, f) = foldl writeUnicodeChar writeNothing s
    in Utf8Builder $ B.fromUnsafeWrite l f

-- | /O(n)./ Convert a Haskell 'String' to a builder. Unlike 'fromHtmlString',
-- this function will not do any escaping.
--
fromPreEscapedString :: String -> Utf8Builder
fromPreEscapedString s =
    let (l, f) = foldl writePreEscapedUnicodeChar writeNothing s
    in Utf8Builder $ B.fromUnsafeWrite l f

-- | /O(n)./ Convert the builder to a 'L.ByteString'.
--
toLazyByteString :: Utf8Builder -> L.ByteString
toLazyByteString (Utf8Builder builder) = B.toLazyByteString builder

-- | Function to create an empty write. This is used as initial value for folds.
--
writeNothing :: (Int, Ptr Word8 -> IO ())
writeNothing = (0, const $ return ())
{-# INLINE writeNothing #-}

-- | Write an unicode character to a 'Builder', doing HTML escaping.
--
-- SM: I guess the escaping could be almost as efficient when using a copying
-- fromByteString that is inlined appropriately.
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
writeUnicodeChar (l, f) c = writePreEscapedUnicodeChar (l, f) c
{-# INLINE writeUnicodeChar #-}

-- | Write a Unicode character, encoding it as UTF-8.
--
writePreEscapedUnicodeChar :: (Int, Ptr Word8 -> IO ())  -- ^ Current state.
                        -> Char                       -- ^ Character to write.
                        -> (Int, Ptr Word8 -> IO ())  -- ^ Resulting state.
writePreEscapedUnicodeChar (l, f) c = l `seq` encodeCharUtf8 f1 f2 f3 f4 c
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
{-# INLINE writePreEscapedUnicodeChar #-}

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
