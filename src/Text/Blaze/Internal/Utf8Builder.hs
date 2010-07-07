{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
-- | A module for efficiently constructing a 'Builder'. This module offers more
-- functions than the standard ones, optimized for HTML generation.
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

      -- * Creating Builders from various text representations.
    , fromChar
    , fromText
    , fromString

      -- * Creating Builders from ByteStrings.
    , unsafeFromByteString

      -- * Transformations on the builder.
    , optimizePiece

      -- * Extracting the value from the builder.
    , toLazyByteString
    , toText

      -- * Internal functions to extend the builder.
      -- ** The write type.
    , Write
    , fromUnsafeWrite
    , optimizeWriteBuilder
    , writeList

      -- ** Functions to create a write.
    , writeChar
    , writeByteString
    ) where

import Foreign
import Data.Char (ord)
import Data.Monoid (Monoid (..))
import Prelude hiding (quot)

import Debug.Trace (trace)
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | A newtype definition for the UTF-8 builder monoid.
newtype Utf8Builder = Utf8Builder Builder
    deriving (Monoid)

-- | /O(1)./ Convert a Haskell character to a 'Utf8Builder', without doing any
-- escaping.
--
fromChar :: Char -> Utf8Builder
fromChar = fromUnsafeWrite . writeChar

-- | /O(n)./ Convert a 'Text' value to a 'Utf8Builder'. This function will not
-- do any HTML escaping.
--
fromText :: Text -> Utf8Builder
fromText text = fromUnsafeWrite $
    T.foldl (\w c -> w `mappend` writeChar c) mempty text

-- | /O(n)./ Convert a Haskell 'String' to a builder. Unlike 'fromHtmlString',
-- this function will not do any escaping.
--
fromString :: String -> Utf8Builder
fromString = writeList writeChar
  -- fromUnsafeWrite $
    -- foldl (\w c -> w `mappend` writeChar c) mempty string


-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it. This function is
-- considered unsafe, as a `S.ByteString` can contain invalid UTF-8 bytes, so
-- you chould use it with caution. This function should perform better when
-- dealing with small strings than the fromByteString function from Builder.
--
unsafeFromByteString :: S.ByteString -> Utf8Builder
unsafeFromByteString = fromUnsafeWrite . writeByteString

-- | /O(n)./ Optimize a small builder. This function has an initial speed
-- penalty, but will speed up later calls of the optimized builder piece. This
-- speedup will only work well for small builders (less than 1k characters).
--
optimizePiece :: Utf8Builder -> Utf8Builder
optimizePiece = fromUnsafeWrite . optimizeWriteBuilder
{-# INLINE optimizePiece #-}

-- | /O(n)./ Convert the builder to a 'L.ByteString'.
--
toLazyByteString :: Utf8Builder -> L.ByteString
toLazyByteString (Utf8Builder builder) = B.toLazyByteString builder

-- | /O(n)./ Convert the builder to a 'Text' value. Please note that this
-- function is a lot slower than the 'toLazyByteString' function.
--
toText :: Utf8Builder -> Text
toText = T.concat . map T.decodeUtf8 . L.toChunks . toLazyByteString

-- | Abstract representation of a write action to the internal buffer.
--
data Write = Write
    {-# UNPACK #-} !Int
    (Ptr Word8 -> IO ())

-- Create a monoid interface for the write actions.
instance Monoid Write where
    mempty = Write 0 (const $ return ())
    {-# INLINE mempty #-}
    mappend (Write l1 f1) (Write l2 f2) =
        Write (l1 + l2) (\ptr -> f1 ptr >> f2 (ptr `plusPtr` l1))
    {-# INLINE mappend #-}

-- INV: The writes must be smaller than the default buffer size.
--
-- SM: Note that moving the control flow away from the Builder will give us the
-- next level of speed. This way we have simple tail-recursive functions
-- consuming data and filling the buffer.
writeList :: (a -> Write) -> [a] -> Utf8Builder
writeList f xs0 = Utf8Builder $ B.fillBuffer (go xs0 0)
  where
    go []         !w !l !p = return (w, Nothing) -- here should come the call to the next filler.
    go xs@(x:xs') !w !l !p  = case f x of
      Write n g 
        | n <= l -> do
            g p
            go xs' (w+n) (l-n) (p `plusPtr` n)
        | otherwise ->
            return (w, Just (B.forceNewBuffer `mappend` B.fillBuffer (go xs 0)))
    {-# INLINE go #-}
{-# INLINE writeList #-}

-- | Create a builder from a write.
--
fromUnsafeWrite :: Write        -- ^ Write to execute.
                -> Utf8Builder  -- ^ Resulting builder.
fromUnsafeWrite (Write l f) = Utf8Builder $ B.fromUnsafeWrite l f 
{-# INLINE fromUnsafeWrite #-}

-- | Optimize a small builder to a write operation.
--
optimizeWriteBuilder :: Utf8Builder  -- ^ Small builder to optimize.
                     -> Write        -- ^ Resulting write.
optimizeWriteBuilder = writeByteString . mconcat . L.toChunks . toLazyByteString
{-# INLINE optimizeWriteBuilder #-}

-- | Write a 'S.ByteString' to the builder.
--
writeByteString :: S.ByteString  -- ^ ByteString to write.
                -> Write         -- ^ Resulting write.
writeByteString byteString = Write l f
  where
    (fptr, o, l) = S.toForeignPtr byteString
    f dst = do copyBytes dst (unsafeForeignPtrToPtr fptr `plusPtr` o) l
               touchForeignPtr fptr
    {-# INLINE f #-}
{-# INLINE writeByteString #-}

-- | Write a Unicode character, encoding it as UTF-8.
--
writeChar :: Char   -- ^ Character to write.
          -> Write  -- ^ Resulting write.
writeChar = encodeCharUtf8 f1 f2 f3 f4
  where
    f1 x = Write 1 $ \ptr -> poke ptr x

    f2 x1 x2 = Write 2 $ \ptr -> do poke ptr x1
                                    poke (ptr `plusPtr` 1) x2

    f3 x1 x2 x3 = Write 3 $ \ptr -> do poke ptr x1
                                       poke (ptr `plusPtr` 1) x2
                                       poke (ptr `plusPtr` 2) x3

    f4 x1 x2 x3 x4 = Write 4 $ \ptr -> do poke ptr x1
                                          poke (ptr `plusPtr` 1) x2
                                          poke (ptr `plusPtr` 2) x3
                                          poke (ptr `plusPtr` 3) x4
{-# INLINE writeChar #-}

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
    x | x <= 0x7F -> f1 $ fromIntegral x
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
