--
-- benchmark NewBinary
--

module Main where

import System.IO
import Data.Word
import NewBinary

import Control.Exception
import System.CPUTime
import Numeric

mb :: Int
mb = 10

main :: IO ()
main = sequence_ 
  [ test wordSize chunkSize mb
  | wordSize  <- [1,2,4,8]
  , chunkSize <- [1,2,4,8,16] ]

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)

test :: Int -> Int -> Int -> IO ()
test wordSize chunkSize mb = do
    let bytes :: Int
        bytes = mb * 2^20
        iterations = bytes `div` wordSize
    putStr $ show mb ++ "MB of Word" ++ show (8 * wordSize)
          ++ " in chunks of " ++ show chunkSize ++ ": "
    h <- openBinMem bytes undefined
    start <- tellBin h
    putSeconds <- time $ do
      doPut wordSize chunkSize h iterations
--      BinPtr n _ <- tellBin h
--      print n
    getSeconds <- time $ do
      seekBin h start
      sum <- doGet wordSize chunkSize h iterations
      evaluate sum
--      BinPtr n _ <- tellBin h
--      print (n, sum)
    let putThroughput = fromIntegral mb / putSeconds
        getThroughput = fromIntegral mb / getSeconds
    putStrLn $ showFFloat (Just 2) putThroughput "MB/s write, "
            ++ showFFloat (Just 2) getThroughput "MB/s read"

doPut :: Int -> Int -> BinHandle -> Int -> IO ()
doPut wordSize chunkSize =
  case (wordSize, chunkSize) of
    (1, 1)  -> putWord8N1
    (1, 2)  -> putWord8N2
    (1, 4)  -> putWord8N4
    (1, 8)  -> putWord8N8
    (1, 16) -> putWord8N16
    (2, 1)  -> putWord16N1
    (2, 2)  -> putWord16N2
    (2, 4)  -> putWord16N4
    (2, 8)  -> putWord16N8
    (2, 16) -> putWord16N16
    (4, 1)  -> putWord32N1
    (4, 2)  -> putWord32N2
    (4, 4)  -> putWord32N4
    (4, 8)  -> putWord32N8
    (4, 16) -> putWord32N16
    (8, 1)  -> putWord64N1
    (8, 2)  -> putWord64N2
    (8, 4)  -> putWord64N4
    (8, 8)  -> putWord64N8
    (8, 16) -> putWord64N16

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 = put_
{-# INLINE putWord8 #-}

putWord16be :: BinHandle -> Word16 -> IO ()
putWord16be = put_
{-# INLINE putWord16be #-}

putWord32be :: BinHandle -> Word32 -> IO ()
putWord32be = put_
{-# INLINE putWord32be #-}

putWord64be :: BinHandle -> Word64 -> IO ()
putWord64be = put_
{-# INLINE putWord64be #-}

getWord8 :: BinHandle -> IO Word8
getWord8 = get
{-# INLINE getWord8 #-}

getWord16be :: BinHandle -> IO Word16
getWord16be = get
{-# INLINE getWord16be #-}

getWord32be :: BinHandle -> IO Word32
getWord32be = get
{-# INLINE getWord32be #-}

getWord64be :: BinHandle -> IO Word64
getWord64be = get
{-# INLINE getWord64be #-}

putWord8N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          loop (s+1) (n-1)

putWord8N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          loop (s+2) (n-2)

putWord8N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          loop (s+4) (n-4)

putWord8N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          putWord8 hnd (s+4)
          putWord8 hnd (s+5)
          putWord8 hnd (s+6)
          putWord8 hnd (s+7)
          loop (s+8) (n-8)

putWord8N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 hnd (s+0)
          putWord8 hnd (s+1)
          putWord8 hnd (s+2)
          putWord8 hnd (s+3)
          putWord8 hnd (s+4)
          putWord8 hnd (s+5)
          putWord8 hnd (s+6)
          putWord8 hnd (s+7)
          putWord8 hnd (s+8)
          putWord8 hnd (s+9)
          putWord8 hnd (s+10)
          putWord8 hnd (s+11)
          putWord8 hnd (s+12)
          putWord8 hnd (s+13)
          putWord8 hnd (s+14)
          putWord8 hnd (s+15)
          loop (s+16) (n-16)


putWord16N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be hnd (s+0)
          loop (s+1) (n-1)

putWord16N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be hnd (s+0)
          putWord16be hnd (s+1)
          loop (s+2) (n-2)

putWord16N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be hnd (s+0)
          putWord16be hnd (s+1)
          putWord16be hnd (s+2)
          putWord16be hnd (s+3)
          loop (s+4) (n-4)

putWord16N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be hnd (s+0)
          putWord16be hnd (s+1)
          putWord16be hnd (s+2)
          putWord16be hnd (s+3)
          putWord16be hnd (s+4)
          putWord16be hnd (s+5)
          putWord16be hnd (s+6)
          putWord16be hnd (s+7)
          loop (s+8) (n-8)

putWord16N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be hnd (s+0)
          putWord16be hnd (s+1)
          putWord16be hnd (s+2)
          putWord16be hnd (s+3)
          putWord16be hnd (s+4)
          putWord16be hnd (s+5)
          putWord16be hnd (s+6)
          putWord16be hnd (s+7)
          putWord16be hnd (s+8)
          putWord16be hnd (s+9)
          putWord16be hnd (s+10)
          putWord16be hnd (s+11)
          putWord16be hnd (s+12)
          putWord16be hnd (s+13)
          putWord16be hnd (s+14)
          putWord16be hnd (s+15)
          loop (s+16) (n-16)


putWord32N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be hnd (s+0)
          loop (s+1) (n-1)

putWord32N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be hnd (s+0)
          putWord32be hnd (s+1)
          loop (s+2) (n-2)

putWord32N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be hnd (s+0)
          putWord32be hnd (s+1)
          putWord32be hnd (s+2)
          putWord32be hnd (s+3)
          loop (s+4) (n-4)

putWord32N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be hnd (s+0)
          putWord32be hnd (s+1)
          putWord32be hnd (s+2)
          putWord32be hnd (s+3)
          putWord32be hnd (s+4)
          putWord32be hnd (s+5)
          putWord32be hnd (s+6)
          putWord32be hnd (s+7)
          loop (s+8) (n-8)

putWord32N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be hnd (s+0)
          putWord32be hnd (s+1)
          putWord32be hnd (s+2)
          putWord32be hnd (s+3)
          putWord32be hnd (s+4)
          putWord32be hnd (s+5)
          putWord32be hnd (s+6)
          putWord32be hnd (s+7)
          putWord32be hnd (s+8)
          putWord32be hnd (s+9)
          putWord32be hnd (s+10)
          putWord32be hnd (s+11)
          putWord32be hnd (s+12)
          putWord32be hnd (s+13)
          putWord32be hnd (s+14)
          putWord32be hnd (s+15)
          loop (s+16) (n-16)

putWord64N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be hnd (s+0)
          loop (s+1) (n-1)

putWord64N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be hnd (s+0)
          putWord64be hnd (s+1)
          loop (s+2) (n-2)

putWord64N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be hnd (s+0)
          putWord64be hnd (s+1)
          putWord64be hnd (s+2)
          putWord64be hnd (s+3)
          loop (s+4) (n-4)

putWord64N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be hnd (s+0)
          putWord64be hnd (s+1)
          putWord64be hnd (s+2)
          putWord64be hnd (s+3)
          putWord64be hnd (s+4)
          putWord64be hnd (s+5)
          putWord64be hnd (s+6)
          putWord64be hnd (s+7)
          loop (s+8) (n-8)

putWord64N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be hnd (s+0)
          putWord64be hnd (s+1)
          putWord64be hnd (s+2)
          putWord64be hnd (s+3)
          putWord64be hnd (s+4)
          putWord64be hnd (s+5)
          putWord64be hnd (s+6)
          putWord64be hnd (s+7)
          putWord64be hnd (s+8)
          putWord64be hnd (s+9)
          putWord64be hnd (s+10)
          putWord64be hnd (s+11)
          putWord64be hnd (s+12)
          putWord64be hnd (s+13)
          putWord64be hnd (s+14)
          putWord64be hnd (s+15)
          loop (s+16) (n-16)

doGet :: Int -> Int -> BinHandle -> Int ->  IO Int
doGet wordSize chunkSize hnd =
  case (wordSize, chunkSize) of
    (1, 1)  -> fmap fromIntegral . getWord8N1 hnd
    (1, 2)  -> fmap fromIntegral . getWord8N2 hnd
    (1, 4)  -> fmap fromIntegral . getWord8N4 hnd
    (1, 8)  -> fmap fromIntegral . getWord8N8 hnd
    (1, 16) -> fmap fromIntegral . getWord8N16 hnd
    (2, 1)  -> fmap fromIntegral . getWord16N1 hnd
    (2, 2)  -> fmap fromIntegral . getWord16N2 hnd
    (2, 4)  -> fmap fromIntegral . getWord16N4 hnd
    (2, 8)  -> fmap fromIntegral . getWord16N8 hnd
    (2, 16) -> fmap fromIntegral . getWord16N16 hnd
    (4, 1)  -> fmap fromIntegral . getWord32N1 hnd
    (4, 2)  -> fmap fromIntegral . getWord32N2 hnd
    (4, 4)  -> fmap fromIntegral . getWord32N4 hnd
    (4, 8)  -> fmap fromIntegral . getWord32N8 hnd
    (4, 16) -> fmap fromIntegral . getWord32N16 hnd
    (8, 1)  -> fmap fromIntegral . getWord64N1 hnd
    (8, 2)  -> fmap fromIntegral . getWord64N2 hnd
    (8, 4)  -> fmap fromIntegral . getWord64N4 hnd
    (8, 8)  -> fmap fromIntegral . getWord64N8 hnd
    (8, 16) -> fmap fromIntegral . getWord64N16 hnd

getWord8N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8 hnd
          loop (s+s0) (n-1)

getWord8N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8 hnd
          s1 <- getWord8 hnd
          loop (s+s0+s1) (n-2)

getWord8N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8 hnd
          s1 <- getWord8 hnd
          s2 <- getWord8 hnd
          s3 <- getWord8 hnd
          loop (s+s0+s1+s2+s3) (n-4)

getWord8N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8 hnd
          s1 <- getWord8 hnd
          s2 <- getWord8 hnd
          s3 <- getWord8 hnd
          s4 <- getWord8 hnd
          s5 <- getWord8 hnd
          s6 <- getWord8 hnd
          s7 <- getWord8 hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord8N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8 hnd
          s1 <- getWord8 hnd
          s2 <- getWord8 hnd
          s3 <- getWord8 hnd
          s4 <- getWord8 hnd
          s5 <- getWord8 hnd
          s6 <- getWord8 hnd
          s7 <- getWord8 hnd
          s8 <- getWord8 hnd
          s9 <- getWord8 hnd
          s10 <- getWord8 hnd
          s11 <- getWord8 hnd
          s12 <- getWord8 hnd
          s13 <- getWord8 hnd
          s14 <- getWord8 hnd
          s15 <- getWord8 hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)


getWord16N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be hnd
          loop (s+s0) (n-1)

getWord16N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be hnd
          s1 <- getWord16be hnd
          loop (s+s0+s1) (n-2)

getWord16N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be hnd
          s1 <- getWord16be hnd
          s2 <- getWord16be hnd
          s3 <- getWord16be hnd
          loop (s+s0+s1+s2+s3) (n-4)

getWord16N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be hnd
          s1 <- getWord16be hnd
          s2 <- getWord16be hnd
          s3 <- getWord16be hnd
          s4 <- getWord16be hnd
          s5 <- getWord16be hnd
          s6 <- getWord16be hnd
          s7 <- getWord16be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord16N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be hnd
          s1 <- getWord16be hnd
          s2 <- getWord16be hnd
          s3 <- getWord16be hnd
          s4 <- getWord16be hnd
          s5 <- getWord16be hnd
          s6 <- getWord16be hnd
          s7 <- getWord16be hnd
          s8 <- getWord16be hnd
          s9 <- getWord16be hnd
          s10 <- getWord16be hnd
          s11 <- getWord16be hnd
          s12 <- getWord16be hnd
          s13 <- getWord16be hnd
          s14 <- getWord16be hnd
          s15 <- getWord16be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)


getWord32N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be hnd
          loop (s+s0) (n-1)

getWord32N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be hnd
          s1 <- getWord32be hnd
          loop (s+s0+s1) (n-2)

getWord32N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be hnd
          s1 <- getWord32be hnd
          s2 <- getWord32be hnd
          s3 <- getWord32be hnd
          loop (s+s0+s1+s2+s3) (n-4)

getWord32N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be hnd
          s1 <- getWord32be hnd
          s2 <- getWord32be hnd
          s3 <- getWord32be hnd
          s4 <- getWord32be hnd
          s5 <- getWord32be hnd
          s6 <- getWord32be hnd
          s7 <- getWord32be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord32N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be hnd
          s1 <- getWord32be hnd
          s2 <- getWord32be hnd
          s3 <- getWord32be hnd
          s4 <- getWord32be hnd
          s5 <- getWord32be hnd
          s6 <- getWord32be hnd
          s7 <- getWord32be hnd
          s8 <- getWord32be hnd
          s9 <- getWord32be hnd
          s10 <- getWord32be hnd
          s11 <- getWord32be hnd
          s12 <- getWord32be hnd
          s13 <- getWord32be hnd
          s14 <- getWord32be hnd
          s15 <- getWord32be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

getWord64N1 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be hnd
          loop (s+s0) (n-1)

getWord64N2 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be hnd
          s1 <- getWord64be hnd
          loop (s+s0+s1) (n-2)

getWord64N4 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be hnd
          s1 <- getWord64be hnd
          s2 <- getWord64be hnd
          s3 <- getWord64be hnd
          loop (s+s0+s1+s2+s3) (n-4)

getWord64N8 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be hnd
          s1 <- getWord64be hnd
          s2 <- getWord64be hnd
          s3 <- getWord64be hnd
          s4 <- getWord64be hnd
          s5 <- getWord64be hnd
          s6 <- getWord64be hnd
          s7 <- getWord64be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord64N16 hnd = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be hnd
          s1 <- getWord64be hnd
          s2 <- getWord64be hnd
          s3 <- getWord64be hnd
          s4 <- getWord64be hnd
          s5 <- getWord64be hnd
          s6 <- getWord64be hnd
          s7 <- getWord64be hnd
          s8 <- getWord64be hnd
          s9 <- getWord64be hnd
          s10 <- getWord64be hnd
          s11 <- getWord64be hnd
          s12 <- getWord64be hnd
          s13 <- getWord64be hnd
          s14 <- getWord64be hnd
          s15 <- getWord64be hnd
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)
