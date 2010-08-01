-- | A module that extends the builder monoid from BlazeHtml with a number of
-- functions to insert unicode as UTF-8.
--
module Text.Blaze.Builder.Utf8
    ( 
      -- * Custom writes to the builder
      writeChar

      -- * Creating builders
    , fromChar
    , fromString
    , fromText
    ) where

import Foreign
import Data.Char (ord)
import Data.Monoid (mempty, mappend)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Builder.Core

-- | Write a Unicode character, encoding it as UTF-8.
--
writeChar :: Char   -- ^ Character to write
          -> Write  -- ^ Resulting write
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
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
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

-- | An unescaped, utf8 encoded character.
--
fromChar :: Char     -- ^ 'Char' to insert
         -> Builder  -- ^ Resulting 'Builder'
fromChar = writeSingleton writeChar

-- | A list of unescaped, utf8 encoded characters.
--
fromString :: String   -- ^ 'String' to insert
           -> Builder  -- ^ Resulting 'Builder'
fromString = writeList writeChar

-- | Create an UTF-8 encoded 'Builder' from some 'Text'.
--
fromText :: Text     -- ^ 'Text' to insert
         -> Builder  -- ^ Resulting 'Builder'
fromText = writeSingleton (T.foldl (\w c -> w `mappend` writeChar c) mempty)
