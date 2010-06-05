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

import Control.Concurrent (forkIO)
import Data.Monoid (mappend)
import Control.Applicative ((<$>))
import Control.Monad (forever, mapM_)
import Network.Socket (listen, accept, sClose)
import Network (listenOn, PortID (PortNumber))
import System (getArgs)
import Data.Char (ord)

import Network.Socket.ByteString (recv, send, sendMany)
import Network.Socket.ByteString.Lazy (sendAll)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB

import Utf8Html hiding (main)

main :: IO ()
main = do
    port <- PortNumber . fromIntegral . read . head <$> getArgs
    socket <- listenOn port
    forever $ do
        (s, _) <- accept socket 
        forkIO (respond s)
  where
    respond s = do
        string <- recv s 1024
        let words = SB.split (fromIntegral $ ord ' ') string
            requestUrl = words !! 1
        _ <- send s $ "HTTP/1.1 200 OK\r\n"
            `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
            `mappend` "\r\n"
        -- SM: Perhaps this ^^ could also be added directly to the
        -- builder.  Thus saving a system call and getting some more
        -- speed.
        case SB.split (fromIntegral $ ord '/') requestUrl of
            (_ : h : w : _) -> do
                let height = read $ SBC.unpack h
                    width = read $ SBC.unpack w
                    rows = [1 .. width]
                    matrix = replicate height rows
                sendAll s $ bigTable matrix
                -- SM: using `sendAll` makes it already a bit faster. However,
                -- currently we are still spending way too much time in the
                -- server part.
                --
                -- A simple optimization is in network-bytestring.Lazy
                --
                -- 
                -- sendAll :: Socket      -- ^ Connected socket
                --         -> ByteString  -- ^ Data to send
                --         -> IO ()
                -- sendAll sock bs = do
                --   sent <- send sock bs
                --   when (sent < L.length bs) $ sendAll sock (L.drop sent bs)
                --
                --                ^ here the the length is computed twice ^
                --                ( event worse: the lazy bytestring is forced
                --                  completely! )
                --
                -- a more efficient version is:
                -- ----------------------------
                --
                -- sendAll sock bs = do
                --   sent <- send sock bs
                --   let bs' = L.drop sent bs
                --   when (L.null bs') $ sendAll sock bs'
                --
            -- For any other URL, send a fixed-size matrix.
            _ -> do
                sendAll s $ bigTable $ replicate 1000 [1 .. 50]
        sClose s
