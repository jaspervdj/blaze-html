{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.NewBuilder
-- Copyright   : Lennart Kolmodin, Ross Paterson
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Efficient construction of lazy bytestrings.
--
-- Modified: June 12th, 2010, Simon Meier <simon.meier@inf.ethz.ch>
--
--   Goal: As little as possible abstraction overhead. Therefore, we've changed
--   the definition of the Builder type such that less data needs to be passed
--   between calls to the build functions filling the buffer.
--
--   The results are pretty convincing for sequencing singleton builders built
--   from a Word8 list. Use 'make bench-new-builder' to check the numbers.
--
--      factor 2.6 speedup from old builder to new builder using singletons
--      factor 9   speedup from old builder to new builder using word8list
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Binary.NewBuilder 
{- (

    -- * The NewBuilder type
      NewBuilder
    , toLazyByteString

    -- * Constructing NewBuilders
    , empty
    , singleton
    , append
    , fromByteString        -- :: S.ByteString -> NewBuilder
    , fromLazyByteString    -- :: L.ByteString -> NewBuilder
    , fromUnsafeWrite
    , fillBuffer
    , forceNewBuffer

    -- * Flushing the buffer state
    , flush

    -- * Derived NewBuilders
    -- ** Big-endian writes
    , putWord16be           -- :: Word16 -> NewBuilder
    , putWord32be           -- :: Word32 -> NewBuilder
    , putWord64be           -- :: Word64 -> NewBuilder

    -- ** Little-endian writes
    , putWord16le           -- :: Word16 -> NewBuilder
    , putWord32le           -- :: Word32 -> NewBuilder
    , putWord64le           -- :: Word64 -> NewBuilder

    -- ** Host-endian, unaligned writes
    , putWordhost           -- :: Word -> NewBuilder
    , putWord16host         -- :: Word16 -> NewBuilder
    , putWord32host         -- :: Word32 -> NewBuilder
    , putWord64host         -- :: Word64 -> NewBuilder

  )-} where

import Criterion.Main

import Foreign
import Data.Monoid
import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Binary.Builder as B

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..),Word16(..),Word64(..))

#if WORD_SIZE_IN_BITS < 64 && __GLASGOW_HASKELL__ >= 608
import GHC.Word (uncheckedShiftRL64#)
#endif
#endif

------------------------------------------------------------------------

main = defaultMain $
    [ bench "[Word8]/old/singletons" $ nf benchOBsingletons byteData   ] ++
    [ bench "[Word8]/new/singletons" $ nf benchNBsingletons byteData   ] ++
    [ bench "[Word8]/new/word8list" $ nf benchNBOptsingletons byteData ] ++
    [ bench "[Bytestring]/old/fromByteString" $ nf benchOBbyteString byteStringData ] ++
    [ bench "[Bytestring]/new/fromByteString" $ nf benchNBbyteString byteStringData ] ++
    [ bench "[Bytestring]/new/byteStringList" $ nf benchNBOptbyteString byteStringData ] ++
    []

benchOBsingletons :: [Word8] -> Int64
benchOBsingletons = L.length . B.toLazyByteString . mconcat . map B.singleton

benchNBsingletons :: [Word8] -> Int64
benchNBsingletons = L.length . toLazyByteString . mconcat . map singleton

benchNBOptsingletons :: [Word8] -> Int64
benchNBOptsingletons = L.length . toLazyByteString . listWord8

benchNBbyteString :: [S.ByteString] -> Int64
benchNBbyteString = L.length . toLazyByteString . mconcat . map copyByteString

benchNBOptbyteString :: [S.ByteString] -> Int64
benchNBOptbyteString = L.length . toLazyByteString . listCopyByteString

benchOBbyteString :: [S.ByteString] -> Int64
benchOBbyteString = L.length . B.toLazyByteString . mconcat . map B.copyByteString


repetitions :: Int
repetitions = 100000

byteData :: [Word8]
byteData = take repetitions $ cycle [1..]
{-# NOINLINE byteData #-}

byteStringData :: [S.ByteString]
byteStringData = replicate 20000 "<img>"
{-# NOINLINE byteStringData #-}

------------------------------------------------------------------------

-- | A 'NewBuilder' is an efficient way to build lazy 'L.ByteString's.
-- There are several functions for constructing 'NewBuilder's, but only one
-- to inspect them: to extract any data, you have to turn them into lazy
-- 'L.ByteString's using 'toLazyByteString'.
--
-- Internally, a 'NewBuilder' constructs a lazy 'L.Bytestring' by filling byte
-- arrays piece by piece.  As each buffer is filled, it is \'popped\'
-- off, to become a new chunk of the resulting lazy 'L.ByteString'.
-- All this is hidden from the user of the 'NewBuilder'.

data BuildSignal =
    -- | Signal the completion of the write process.
    Done {-# UNPACK #-} !(Ptr Word8) -- ^ Pointer to the next free byte.
    -- | Signal that the buffer is full and a new one needs to be allocated.
  | BufferFull
      {-# UNPACK #-} !Int         -- ^ Minimal size required for the next buffer.
      {-# UNPACK #-} !(Ptr Word8) -- ^ Pointer to the next free byte.
      {-# UNPACK #-} !BuildStep

-- every build step checks that free + bytes-written <= last
type BuildStep =  Ptr Word8      -- ^ Ptr to the next free byte in the buffer.
               -> Ptr Word8      -- ^ Ptr to the first byte AFTER the buffer.
               -> IO BuildSignal -- ^ Signal the next step to be taken.

-- NOTE: Above we are lacking the capability to specify the strictness of the 
-- first two arguments.

newtype NewBuilder = NewBuilder {
      unNewBuilder :: BuildStep -> BuildStep
    }

-- Write abstraction so we can avoid some gory and bloody details.
--
data Write = Write
    {-# UNPACK #-} !Int
    (Ptr Word8 -> IO ())

instance Monoid NewBuilder where
    mempty  = empty
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr append mempty
    {-# INLINE mconcat #-}

------------------------------------------------------------------------
-- Writes 

writeByte :: Word8 -> Write
writeByte x = Write 1 (\pf -> poke pf x)
{-# INLINE writeByte #-}

writeByteString :: S.ByteString -> Write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | /O(n)./ Construct a NewBuilder from a write abstraction.
--
writeSingleton :: (a -> Write) -> a -> NewBuilder
writeSingleton write = mkBuilder
  where 
  mkBuilder x = NewBuilder step
    where
      step k pf pe
        | pf `plusPtr` size <= pe = do 
            io pf
            let pf' = pf `plusPtr` size
            pf' `seq` k pf' pe
        | otherwise               = return $ BufferFull size pf (step k)
        where
        Write size io = write x
{-# INLINE writeSingleton #-}

-- | Construct a builder writing a list of data from a write abstraction.
writeList :: (a -> Write) -> [a] -> NewBuilder
writeList write = mkBuilder
  where
  mkBuilder []  = empty
  mkBuilder xs0 = NewBuilder $ step xs0
    where
      step xs1 k pf0 pe0 = go xs1 pf0
        where
          go []          !pf = k pf pe0
          go xs@(x':xs') !pf
            | pf `plusPtr` size <= pe0  = do 
                io pf
                go xs' (pf `plusPtr` size)
            | otherwise = do return $ BufferFull size pf (step xs k)
            where
            Write size io = write x'
{-# INLINE writeList #-}

------------------------------------------------------------------------

-- | /O(1)./ The empty NewBuilder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: NewBuilder
empty = NewBuilder id
{-# INLINE empty #-}

-- | /O(1)./ A NewBuilder taking a single byte, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Word8 -> NewBuilder
singleton = writeSingleton writeByte

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString -> NewBuilder
copyByteString = writeSingleton writeByteString

-- | Copy the bytes of the list to the buffer of the builder.
listWord8 :: [Word8] -> NewBuilder
listWord8 = writeList writeByte

-- | Copy each bytestring from the list of bytestrings to the buffer of the
-- builder.
listCopyByteString :: [S.ByteString] -> NewBuilder
listCopyByteString = writeList writeByteString


-- copied from Data.ByteString.Lazy
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | Run the builder with the default buffer size.
runBuilder :: NewBuilder -> [S.ByteString] -> [S.ByteString]
runBuilder = genRunBuilder defaultSize

-- | Run the builder with buffers of at least the given size.
--
-- Note that the builders should guarantee that on average the desired buffer
-- size is attained almost perfectly. "Almost" because builders may decide to
-- start a new buffer and not completely fill the existing buffer, if this is
-- faster. However, they should not spill too much of the buffer, if they
-- cannot compensate for it.
genRunBuilder :: Int -> NewBuilder -> [S.ByteString] -> [S.ByteString]
genRunBuilder bufSize (NewBuilder b) k = 
    inlinePerformIO $ go bufSize (b finalStep)
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
                          inlinePerformIO (go (max newSize bufSize) nextStep)

toLazyByteString :: NewBuilder -> L.ByteString
toLazyByteString = L.fromChunks . flip runBuilder []

test = toLazyByteString $ singleton 65 `append` singleton 65

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two NewBuilders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: NewBuilder -> NewBuilder -> NewBuilder
append (NewBuilder f) (NewBuilder g) = NewBuilder (f . g)
{-# INLINE append #-}



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
---
---  OLD CODE FROM standard binary builder
---
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

{-
-- | /O(1)./ A NewBuilder taking a 'S.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromByteString' bs) = 'L.fromChunks' [bs]@
--
fromByteString :: S.ByteString -> NewBuilder
fromByteString bs
  | S.null bs = empty
  | otherwise = flush `append` mapNewBuilder (bs :)
{-# INLINE fromByteString #-}

-- | /O(1)./ A NewBuilder taking a lazy 'L.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromLazyByteString' bs) = bs@
--
fromLazyByteString :: L.ByteString -> NewBuilder
fromLazyByteString bss = flush `append` mapNewBuilder (L.toChunks bss ++)
{-# INLINE fromLazyByteString #-}

-- | /O(n)./ A NewBuilder from a raw write to a pointer.
fromUnsafeWrite :: Int                  -- ^ Number of bytes to be written.
                -> (Ptr Word8 -> IO ()) -- ^ Function that does the write.
                -> NewBuilder              -- ^ Resulting 'NewBuilder'.
fromUnsafeWrite = writeN
{-# INLINE fromUnsafeWrite #-}

------------------------------------------------------------------------

-- Our internal buffer type
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'NewBuilder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteString :: NewBuilder -> L.ByteString
toLazyByteString m = L.fromChunks $ unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (runNewBuilder (m `append` flush) (const []) buf)

-- | /O(1)./ Pop the 'S.ByteString' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.ByteString'.
flush :: NewBuilder
flush = NewBuilder $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)

------------------------------------------------------------------------

------------------------------------------------------------------------

-- | Sequence an IO operation on the buffer
unsafeLiftIO :: (Buffer -> IO Buffer) -> NewBuilder
unsafeLiftIO f =  NewBuilder $ \ k buf -> inlinePerformIO $ do
    buf' <- f buf
    return (k buf')
{-# INLINE unsafeLiftIO #-}

-- | Get the size of the buffer
withSize :: (Int -> NewBuilder) -> NewBuilder
withSize f = NewBuilder $ \ k buf@(Buffer _ _ _ l) ->
    runNewBuilder (f l) k buf

-- | Map the resulting list of bytestrings.
mapNewBuilder :: ([S.ByteString] -> [S.ByteString]) -> NewBuilder
mapNewBuilder f = NewBuilder (f .)

------------------------------------------------------------------------

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> NewBuilder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))
{-# INLINE ensureFree #-}

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> NewBuilder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)
{-# INLINE writeN #-}

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))
{-# INLINE writeNBuffer #-}

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size
{-# INLINE newBuffer #-}

-- SM: A function that allows for handing over control flow to the given
-- function. 
--
--   The arguments are: length available, ptr to first byte.
--   The results are  : the number of written bytes and a builder to be
--                      executed for appending the remaining data.
--
-- NOTE: The returned builder must not reference the given Ptr Word8 anymore.
--
-- NOTE: There may be nicer constructions for achieving the same effect. This
-- is just a first hack and I'm not 100% sure that it will always work
-- correctly. That still needs to be verified.
fillBuffer :: (Int -> Ptr Word8 -> IO (Int, Maybe NewBuilder)) -> NewBuilder
fillBuffer f = NewBuilder $ \ k buf@(Buffer fp o u l) -> inlinePerformIO $ do
    (n, next) <- withForeignPtr fp (\p -> f l (p `plusPtr` (o+u)))
    let buf' = Buffer fp o (u+n) (l-n)
    case next of
      Nothing -> return (k buf')
      Just b  -> return (runNewBuilder b k buf')
{-# INLINE fillBuffer #-}

-- | SM: Forces the construction of a new buffer. Currently used in implementations
-- making use of fillBuffer to ensure that a new buffer gets created if no more
-- bytes were free.
forceNewBuffer :: NewBuilder
forceNewBuffer = flush `append` unsafeLiftIO (const (newBuffer defaultSize))
{-# INLINE forceNewBuffer #-}

------------------------------------------------------------------------
-- Aligned, host order writes of storable values

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- storable values into the memory.
writeNbytes :: Storable a => Int -> (Ptr a -> IO ()) -> NewBuilder
writeNbytes n f = ensureFree n `append` unsafeLiftIO (writeNBufferBytes n f)
{-# INLINE writeNbytes #-}

writeNBufferBytes :: Storable a => Int -> (Ptr a -> IO ()) -> Buffer -> IO Buffer
writeNBufferBytes n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))
{-# INLINE writeNBufferBytes #-}

------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> NewBuilder
putWord16be w = writeN 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> NewBuilder
putWord16le w = writeN 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE putWord16le #-}

-- putWord16le w16 = writeN 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> NewBuilder
putWord32be w = writeN 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}

--
-- a data type to tag Put/Check. writes construct these which are then
-- inlined and flattened. matching Checks will be more robust with rules.
--

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> NewBuilder
putWord32le w = writeN 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE putWord32le #-}

-- on a little endian machine:
-- putWord32le w32 = writeN 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> NewBuilder
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
putWord64be w =
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    in writeN 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
putWord64be w = writeN 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> NewBuilder

#if WORD_SIZE_IN_BITS < 64
putWord64le w =
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    in writeN 8 $ \p -> do
    poke (p)             (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
putWord64le w = writeN 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
{-# INLINE putWord64le #-}

-- on a little endian machine:
-- putWord64le w64 = writeN 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A NewBuilder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost :: Word -> NewBuilder
putWordhost w = writeNbytes (sizeOf (undefined :: Word)) (\p -> poke p w)
{-# INLINE putWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
putWord16host :: Word16 -> NewBuilder
putWord16host w16 = writeNbytes (sizeOf (undefined :: Word16)) (\p -> poke p w16)
{-# INLINE putWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putWord32host :: Word32 -> NewBuilder
putWord32host w32 = writeNbytes (sizeOf (undefined :: Word32)) (\p -> poke p w32)
{-# INLINE putWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
putWord64host :: Word64 -> NewBuilder
putWord64host w = writeNbytes (sizeOf (undefined :: Word64)) (\p -> poke p w)
{-# INLINE putWord64host #-}

------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
-}

