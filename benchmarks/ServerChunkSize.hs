-- | A benchmark for measuring the impact of lazy bytestring chunk size on
-- server performance.
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn)

import Control.Concurrent (forkIO)
import Data.Monoid (mappend)
import Control.Monad (forever)
import Network.Socket (accept, sClose)
import Network (listenOn, PortID (PortNumber))
import System (getArgs)
import Data.Char (ord)

import Network.Socket.ByteString (recv, send)
import Network.Socket.ByteString.Lazy (sendAll)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB

-- | Generate a 128k response, with a given chunk size.
--
makeResponse :: Int            -- ^ Chunk size.
             -> LB.ByteString  -- ^ Result.
makeResponse chunkSize =
    let chunks = createChunks chunkSize totalSize
    in LB.fromChunks chunks
  where
    -- A 64 kilobyte response.
    totalSize = 128 * 1024

    createChunks c s
      | c < s     = SBC.replicate c 'a' : createChunks c (s - c)
      | otherwise = SBC.replicate s 'a' : []

main :: IO ()
main = do
    args <- getArgs
    let port = PortNumber $ fromIntegral $ read $ head args
        chunkSize = read $ args !! 1

    socket <- listenOn port
    forever $ do
        (s, _) <- accept socket
        forkIO (respond chunkSize s)
  where
    respond chunkSize s = do
        _ <- recv s 1024
        _ <- send s $ "HTTP/1.1 200 OK\r\n"
            `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
            `mappend` "\r\n"
        sendAll s $ makeResponse chunkSize
        sClose s
