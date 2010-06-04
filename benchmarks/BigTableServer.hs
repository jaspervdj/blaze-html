-- | This is a simple benchmark containing some IO. This program runs as a HTTP
-- server on a given port. It responds by spamming big tables in the right size.
-- The request protocol for clients is:
--
-- > GET /rows/columns HTTP/1.1
--
-- For example,
--
-- > GET /1000/10 HTTP/1.1
--
-- The server then responds with an HTML table with the requested dimensions.
--
{-# LANGUAGE OverloadedStrings #-}
module BigTableServer where

import Prelude hiding (putStrLn)

import Data.Monoid (mappend)
import Control.Applicative ((<$>))
import Control.Monad (forever, mapM_)
import Network.Socket (listen, accept, sClose)
import Network (listenOn, PortID (PortNumber))
import System (getArgs)
import Data.Char (ord)

import Network.Socket.ByteString (recv, send, sendMany)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB

import Utf8Html hiding (main)

main :: IO ()
main = do
    port <- PortNumber . fromIntegral . read . head <$> getArgs
    socket <- listenOn port
    forever $ respond socket
  where
    respond socket = do
        (s, _) <- accept socket 
        string <- recv s 1024
        let words = SB.split (fromIntegral $ ord ' ') string
            requestUrl = words !! 1
        case SB.split (fromIntegral $ ord '/') requestUrl of
            (_ : h : w : _) -> do
                let height = read $ SBC.unpack h
                    width = read $ SBC.unpack w
                    rows = [1 .. width]
                    matrix = replicate height rows
                _ <- send s $ "HTTP/1.1 200 OK\r\n"
                    `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
                    `mappend` "\r\n"
                sendMany s $ LB.toChunks $ bigTable matrix
            _ -> do
                _<- send s $ "HTTP/1.1 404 Not Found\r\n"
                    `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
                    `mappend` "\r\n"
                    `mappend` "<h1>Not Found</h1>\r\n"
                return ()
        sClose s
