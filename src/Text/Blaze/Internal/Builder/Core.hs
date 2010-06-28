-- | The builder monoid from BlazeHtml.
--
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Blaze.Internal.Builder.Core where

import Foreign
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.ByteString.Char8 ()
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)

-- | Main builder type.
--
newtype Builder = Builder
    { -- ^ Extract the data.
      unBuilder :: BuildStep -> BuildStep
    }

-- | A buildsignal is a signal returned from a write to the builder, it tells us
-- what should happen next.
--
data BuildSignal
  -- | Signal the completion of the write process.
  = Done {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte.
  -- | Signal that the buffer is full and a new one needs to be allocated.
  | BufferFull
      {-# UNPACK #-} !Int          -- ^ Minimal size required for next buffer.
      {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte.
      {-# UNPACK #-} !BuildStep    -- ^ Continuation.

-- | Type for a single build step. Every build step checks that
--
-- > free + bytes-written <= last
--
type BuildStep =  Ptr Word8       -- ^ Ptr to the next free byte in the buffer.
               -> Ptr Word8       -- ^ Ptr to the first byte AFTER the buffer.
               -> IO BuildSignal  -- ^ Signal the next step to be taken.
instance Monoid Builder where
    mempty  = empty
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr append mempty
    {-# INLINE mconcat #-}

-- | Write abstraction so we can avoid some gory and bloody details.
--
data Write = Write
    {-# UNPACK #-} !Int   -- ^ Exact size of the write, in bytes.
    (Ptr Word8 -> IO ())  -- ^ Function to carry out the write.

-- | Write a single byte.
--
writeByte :: Word8  -- ^ Byte to write.
          -> Write  -- ^ Resulting write.
writeByte x = Write 1 (\pf -> poke pf x)
{-# INLINE writeByte #-}

-- | Write a strict 'S.ByteString'.
--
writeByteString :: S.ByteString  -- ^ 'S.ByteString' to write.
                -> Write         -- ^ Resulting write.
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Construct a 'Builder' from a single 'Write' abstraction.
--
writeSingleton :: (a -> Write) -> a -> Builder
writeSingleton write x = Builder step
  where 
    Write size io = write x
    step k pf pe
      | pf `plusPtr` size <= pe = do 
        io pf
        let pf' = pf `plusPtr` size
        pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
{-# INLINE writeSingleton #-}

-- | Construct a builder writing a list of data from a write abstraction.
--
writeList :: (a -> Write) -> [a] -> Builder
writeList write [] = empty
writeList write xs0 = Builder $ step xs0
  where
    step xs1 k pf0 pe0 = go xs1 pf0
      where
        go []          !pf = k pf pe0
        go xs@(x':xs') !pf
          | pf `plusPtr` size <= pe0  = io pf >> go xs' (pf `plusPtr` size)
          | otherwise = do return $ BufferFull size pf (step xs k)
          where
            Write size io = write x'
{-# INLINE writeList #-}

------------------------------------------------------------------------

-- | /O(1)./ The empty Builder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: Builder
empty = Builder id
{-# INLINE empty #-}

-- | /O(1)./ A Builder taking a single byte, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Word8 -> Builder
singleton = writeSingleton writeByte

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString -> Builder
copyByteString = writeSingleton writeByteString

-- | Copy the bytes of the list to the buffer of the builder.
listWord8 :: [Word8] -> Builder
listWord8 = writeList writeByte

-- | Copy each bytestring from the list of bytestrings to the buffer of the
-- builder.
listCopyByteString :: [S.ByteString] -> Builder
listCopyByteString = writeList writeByteString


-- | Copied from Data.ByteString.Lazy.
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | Run the builder with the default buffer size.
--
runBuilder :: Builder -> [S.ByteString] -> [S.ByteString]
runBuilder = runBuilderWith defaultSize

-- | Run the builder with buffers of at least the given size.
--
-- Note that the builders should guarantee that on average the desired buffer
-- size is attained almost perfectly. "Almost" because builders may decide to
-- start a new buffer and not completely fill the existing buffer, if this is
-- faster. However, they should not spill too much of the buffer, if they
-- cannot compensate for it.
--
runBuilderWith :: Int -> Builder -> [S.ByteString] -> [S.ByteString]
runBuilderWith bufSize (Builder b) k = 
    S.inlinePerformIO $ go bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf
    go !size !step = do
       buf <- S.mallocByteString size
       withForeignPtr buf $ \pf -> do
         next <- step pf (pf `plusPtr` size)
         case next of
           Done pf' | pf == pf' -> return k
                    | otherwise -> return $ S.PS buf 0 (pf' `minusPtr` pf) : k 
           BufferFull newSize pf' nextStep
             | pf == pf' -> error "runBuilder: buffer cannot be full; no data was written."
             | otherwise ->
                 return $ S.PS buf 0 (pf' `minusPtr` pf) : 
                          S.inlinePerformIO (go (max newSize bufSize) nextStep)

toLazyByteString :: Builder -> L.ByteString
toLazyByteString = L.fromChunks . flip runBuilder []

-- | /O(1)./ The concatenation of two NewBuilders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE append #-}
