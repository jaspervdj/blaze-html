{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Binary.Get (

    -- * The Get type
      Get
    , runGet
    , runGetState

    -- * Parsing
    , skip
    , uncheckedSkip
    , lookAhead
    , lookAheadM
    , lookAheadE
    , uncheckedLookAhead

    -- * Utility
    , bytesRead
    , getBytes
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

  ) where

import Control.Monad (when,liftM,ap)
import Control.Monad.Fix
import Data.Maybe (isNothing)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as B
#else
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as L
#endif

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative (Applicative(..))
#endif

import Foreign

-- used by splitAtST
import Control.Monad.ST
import Data.STRef

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
import GHC.Int
#endif

-- | The parse state
data S = S {-# UNPACK #-} !B.ByteString  -- current chunk
           L.ByteString                  -- the rest of the input
           {-# UNPACK #-} !Int64         -- bytes read

-- | The Get monad is just a State monad carrying around the input ByteString
-- We treat it as a strict state monad. 
newtype Get a = Get { unGet :: S -> (# a, S #) }

instance Functor Get where
    fmap f m = Get (\s -> case unGet m s of
                             (# a, s' #) -> (# f a, s' #))
    {-# INLINE fmap #-}

#ifdef APPLICATIVE_IN_BASE
instance Applicative Get where
    pure  = return
    (<*>) = ap
#endif

-- Definition directly from Control.Monad.State.Strict
instance Monad Get where
    return a  = Get $ \s -> (# a, s #)
    {-# INLINE return #-}

    m >>= k   = Get $ \s -> case unGet m s of
                             (# a, s' #) -> unGet (k a) s'
    {-# INLINE (>>=) #-}

    fail      = failDesc

instance MonadFix Get where
    mfix f = Get $ \s -> let (a,s') = case unGet (f a) s of
                                              (# a', s'' #) -> (a',s'')
                        in (# a,s' #)

------------------------------------------------------------------------

get :: Get S
get   = Get $ \s -> (# s, s #)

put :: S -> Get ()
put s = Get $ \_ -> (# (), s #)

------------------------------------------------------------------------
--
-- dons, GHC 6.10: explicit inlining disabled, was killing performance.
-- Without it, GHC seems to do just fine. And we get similar
-- performance with 6.8.2 anyway.
--

initState :: L.ByteString -> S
initState xs = mkState xs 0
{- INLINE initState -}

{-
initState (B.LPS xs) =
    case xs of
      []      -> S B.empty L.empty 0
      (x:xs') -> S x (B.LPS xs') 0
-}

#ifndef BYTESTRING_IN_BASE
mkState :: L.ByteString -> Int64 -> S
mkState l = case l of
    L.Empty      -> S B.empty L.empty
    L.Chunk x xs -> S x xs
{- INLINE mkState -}

#else
mkState :: L.ByteString -> Int64 -> S
mkState (B.LPS xs) =
    case xs of
        [] -> S B.empty L.empty
        (x:xs') -> S x (B.LPS xs')
#endif

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> L.ByteString -> a
runGet m str = case unGet m (initState str) of (# a, _ #) -> a

-- | Run the Get monad applies a 'get'-based parser on the input
-- ByteString. Additional to the result of get it returns the number of
-- consumed bytes and the rest of the input.
runGetState :: Get a -> L.ByteString -> Int64 -> (a, L.ByteString, Int64)
runGetState m str off =
    case unGet m (mkState str off) of
      (# a, ~(S s ss newOff) #) -> (a, s `join` ss, newOff)

------------------------------------------------------------------------

failDesc :: String -> Get a
failDesc err = do
    S _ _ bytes <- get
    Get (error (err ++ ". Failed reading at byte position " ++ show bytes))

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN (fromIntegral n) (const ())

-- | Skip ahead @n@ bytes. No error if there isn't enough bytes.
uncheckedSkip :: Int64 -> Get ()
uncheckedSkip n = do
    S s ss bytes <- get
    if fromIntegral (B.length s) >= n
      then put (S (B.drop (fromIntegral n) s) ss (bytes + n))
      else do
        let rest = L.drop (n - fromIntegral (B.length s)) ss
        put $! mkState rest (bytes + n)

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Get a -> Get a
lookAhead ga = do
    s <- get
    a <- ga
    put s
    return a

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    when (isNothing ma) $
        put s
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

-- | Get the next up to @n@ bytes as a lazy ByteString, without consuming them. 
uncheckedLookAhead :: Int64 -> Get L.ByteString
uncheckedLookAhead n = do
    S s ss _ <- get
    if n <= fromIntegral (B.length s)
        then return (L.fromChunks [B.take (fromIntegral n) s])
        else return $ L.take n (s `join` ss)

------------------------------------------------------------------------
-- Utility

-- | Get the total number of bytes read to this point.
bytesRead :: Get Int64
bytesRead = do
    S _ _ b <- get
    return b

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
-- Note that this forces the rest of the input.
remaining :: Get Int64
remaining = do
    S s ss _ <- get
    return (fromIntegral (B.length s) + L.length ss)

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get Bool
isEmpty = do
    S s ss _ <- get
    return (B.null s && L.null ss)

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Get B.ByteString
getByteString n = readN n id
{-# INLINE getByteString #-}

-- | An efficient 'get' method for lazy ByteStrings. Does not fail if fewer than
-- @n@ bytes are left in the input.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n = do
    S s ss bytes <- get
    let big = s `join` ss
    case splitAtST n big of
      (consume, rest) -> do put $ mkState rest (bytes + n)
                            return consume
{-# INLINE getLazyByteString #-}

-- | Get a lazy ByteString that is terminated with a NUL byte. Fails
-- if it reaches the end of input without hitting a NUL.
getLazyByteStringNul :: Get L.ByteString
getLazyByteStringNul = do
    S s ss bytes <- get
    let big = s `join` ss
        (consume, t) = L.break (== 0) big
        (h, rest) = L.splitAt 1 t
    if L.null h
      then fail "too few bytes"
      else do
        put $ mkState rest (bytes + L.length consume + 1)
        return consume
{-# INLINE getLazyByteStringNul #-}

-- | Get the remaining bytes as a lazy ByteString
getRemainingLazyByteString :: Get L.ByteString
getRemainingLazyByteString = do
    S s ss _ <- get
    return (s `join` ss)

------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n = do
    S s ss bytes <- get
    if n <= B.length s
        then do let (consume,rest) = B.splitAt n s
                put $! S rest ss (bytes + fromIntegral n)
                return $! consume
        else
              case L.splitAt (fromIntegral n) (s `join` ss) of
                (consuming, rest) ->
                    do let now = B.concat . L.toChunks $ consuming
                       put $! mkState rest (bytes + fromIntegral n)
                       -- forces the next chunk before this one is returned
                       if (B.length now < n)
                         then
                            fail "too few bytes"
                         else
                            return now
{- INLINE getBytes -}
-- ^ important

#ifndef BYTESTRING_IN_BASE
join :: B.ByteString -> L.ByteString -> L.ByteString
join bb lb
    | B.null bb = lb
    | otherwise = L.Chunk bb lb

#else
join :: B.ByteString -> L.ByteString -> L.ByteString
join bb (B.LPS lb)
    | B.null bb = B.LPS lb
    | otherwise = B.LPS (bb:lb)
#endif
    -- don't use L.append, it's strict in it's second argument :/
{- INLINE join -}

-- | Split a ByteString. If the first result is consumed before the --
-- second, this runs in constant heap space.
--
-- You must force the returned tuple for that to work, e.g.
-- 
-- > case splitAtST n xs of
-- >    (ys,zs) -> consume ys ... consume zs
--
splitAtST :: Int64 -> L.ByteString -> (L.ByteString, L.ByteString)
splitAtST i ps | i <= 0 = (L.empty, ps)
#ifndef BYTESTRING_IN_BASE
splitAtST i ps          = runST (
     do r  <- newSTRef undefined
        xs <- first r i ps
        ys <- unsafeInterleaveST (readSTRef r)
        return (xs, ys))

  where
        first r 0 xs@(L.Chunk _ _) = writeSTRef r xs    >> return L.Empty
        first r _ L.Empty          = writeSTRef r L.Empty >> return L.Empty

        first r n (L.Chunk x xs)
          | n < l     = do writeSTRef r (L.Chunk (B.drop (fromIntegral n) x) xs)
                           return $ L.Chunk (B.take (fromIntegral n) x) L.Empty
          | otherwise = do writeSTRef r (L.drop (n - l) xs)
                           liftM (L.Chunk x) $ unsafeInterleaveST (first r (n - l) xs)

         where l = fromIntegral (B.length x)
#else
splitAtST i (B.LPS ps)  = runST (
     do r  <- newSTRef undefined
        xs <- first r i ps
        ys <- unsafeInterleaveST (readSTRef r)
        return (B.LPS xs, B.LPS ys))

  where first r 0 xs     = writeSTRef r xs >> return []
        first r _ []     = writeSTRef r [] >> return []
        first r n (x:xs)
          | n < l     = do writeSTRef r (B.drop (fromIntegral n) x : xs)
                           return [B.take (fromIntegral n) x]
          | otherwise = do writeSTRef r (L.toChunks (L.drop (n - l) (B.LPS xs)))
                           fmap (x:) $ unsafeInterleaveST (first r (n - l) xs)

         where l = fromIntegral (B.length x)
#endif
{- INLINE splitAtST -}

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Get a
readN n f = fmap f $ getBytes n
{- INLINE readN -}
-- ^ important

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{- INLINE getPtr -}

------------------------------------------------------------------------

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))
{- INLINE getWord8 -}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 1))
{- INLINE getWord16be -}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 0) )
{- INLINE getWord16le -}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 3) )
{- INLINE getWord32be -}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 0) )
{- INLINE getWord32le -}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 7) )
{- INLINE getWord64be -}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 0) )
{- INLINE getWord64le -}

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))
{- INLINE getWordhost -}

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))
{- INLINE getWord16host -}

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))
{- INLINE getWord32host -}

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))
{- INLINE getWord64host -}

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
