-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : stable
-- Portability : Portable to Hugs and GHC. Requires MPTCs
--
-- The Put monad. A monad for efficiently constructing lazy bytestrings.
--
-----------------------------------------------------------------------------

module Data.Binary.Put (

    -- * The Put type
      Put
    , PutM(..)
    , runPut
    , runPutM
    , putBuilder
    , execPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putByteString
    , putLazyByteString

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le

    -- * Host-endian, unaligned writes
    , putWordhost           -- :: Word   -> Put
    , putWord16host         -- :: Word16 -> Put
    , putWord32host         -- :: Word32 -> Put
    , putWord64host         -- :: Word64 -> Put

  ) where

import Data.Monoid
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.Binary.Builder as B

import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative
#endif


------------------------------------------------------------------------

-- XXX Strict in buffer only. 
data PairS a = PairS a {-# UNPACK #-}!Builder

sndS :: PairS a -> Builder
sndS (PairS _ b) = b

-- | The PutM type. A Writer monad over the efficient Builder monoid.
newtype PutM a = Put { unPut :: PairS a }

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()

instance Functor PutM where
        fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w
        {-# INLINE fmap #-}

#ifdef APPLICATIVE_IN_BASE
instance Applicative PutM where
        pure    = return
        m <*> k = Put $
            let PairS f w  = unPut m
                PairS x w' = unPut k
            in PairS (f x) (w `mappend` w')
#endif

-- Standard Writer monad, with aggressive inlining
instance Monad PutM where
    return a = Put $ PairS a mempty
    {-# INLINE return #-}

    m >>= k  = Put $
        let PairS a w  = unPut m
            PairS b w' = unPut (k a)
        in PairS b (w `mappend` w')
    {-# INLINE (>>=) #-}

    m >> k  = Put $
        let PairS _ w  = unPut m
            PairS b w' = unPut k
        in PairS b (w `mappend` w')
    {-# INLINE (>>) #-}

tell :: Builder -> Put
tell b = Put $ PairS () b
{-# INLINE tell #-}

putBuilder :: Builder -> Put
putBuilder = tell
{-# INLINE putBuilder #-}

-- | Run the 'Put' monad
execPut :: PutM a -> Builder
execPut = sndS . unPut
{-# INLINE execPut #-}

-- | Run the 'Put' monad with a serialiser
runPut :: Put -> L.ByteString
runPut = toLazyByteString . sndS . unPut
{-# INLINE runPut #-}

-- | Run the 'Put' monad with a serialiser and get its result
runPutM :: PutM a -> (a, L.ByteString)
runPutM (Put (PairS f s)) = (f, toLazyByteString s)
{-# INLINE runPutM #-}

------------------------------------------------------------------------

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = tell B.flush
{-# INLINE flush #-}

-- | Efficiently write a byte into the output buffer
putWord8            :: Word8 -> Put
putWord8            = tell . B.singleton
{-# INLINE putWord8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: S.ByteString -> Put
putByteString       = tell . B.fromByteString
{-# INLINE putByteString #-}

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: L.ByteString -> Put
putLazyByteString   = tell . B.fromLazyByteString
{-# INLINE putLazyByteString #-}

-- | Write a Word16 in big endian format
putWord16be         :: Word16 -> Put
putWord16be         = tell . B.putWord16be
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le         :: Word16 -> Put
putWord16le         = tell . B.putWord16le
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be         :: Word32 -> Put
putWord32be         = tell . B.putWord32be
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le         :: Word32 -> Put
putWord32le         = tell . B.putWord32le
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be         :: Word64 -> Put
putWord64be         = tell . B.putWord64be
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le         :: Word64 -> Put
putWord64le         = tell . B.putWord64le
{-# INLINE putWord64le #-}

------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost         :: Word -> Put
putWordhost         = tell . B.putWordhost
{-# INLINE putWordhost #-}

-- | /O(1)./ Write a Word16 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord16host       :: Word16 -> Put
putWord16host       = tell . B.putWord16host
{-# INLINE putWord16host #-}

-- | /O(1)./ Write a Word32 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord32host       :: Word32 -> Put
putWord32host       = tell . B.putWord32host
{-# INLINE putWord32host #-}

-- | /O(1)./ Write a Word64 in native host order
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- For portability issues see @putWordhost@.
putWord64host       :: Word64 -> Put
putWord64host       = tell . B.putWord64host
{-# INLINE putWord64host #-}
