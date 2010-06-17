-- | Benchmark server: all benchmarks from 'Utf8Html' can be requested through
-- this server via URL's.
--
-- Example:
--
-- > GET /manyAttributes HTTP/1.1
--
-- Will give you the @manyAttributes@ benchmark. URL's are case sensitive for
-- simplicity reasons.
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn)

import Control.Concurrent (forkIO)
import Data.Monoid (mappend, mconcat)
import Control.Applicative ((<$>))
import Control.Monad (forever, forM_)
import Network.Socket (accept, sClose)
import Network (listenOn, PortID (PortNumber))
import System (getArgs)
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as M

import Network.Socket.ByteString (recv, send)
import Network.Socket.ByteString.Lazy (sendAll)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB

import Utf8Html (HtmlBenchmark (..), benchmarks)

import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

-- | Root template.
--
root :: Html a
root = html $ do
    H.head $ do
        title "BlazeHtml benchmarks"
    body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
        h1 "Benchmarks"
        p $ do
            "This is a server showing BlazeHtml benchmarks, written using"
            " BlazeHtml. You can check out this benchmarks here, but it is"
            " probably more interesting to measure them using a tool like"
            " " >> code "ab" >> " or " >> code "httperf" >> "."
        forM_ benchmarks benchmark

-- | Composable template for a single benchmark.
--
benchmark :: HtmlBenchmark -> Html a
benchmark (HtmlBenchmark name _ _ description) = do
    h2 $ string name
    p $ description
    p $ "URL: " >> code ("/" >> string name)
    p $ a ! href (stringValue name) $ "Go to benchmark."

main :: IO ()
main = do
    port <- PortNumber . fromIntegral . read . head <$> getArgs
    socket <- listenOn port
    forever $ do
        (s, _) <- accept socket
        forkIO (respond s)
  where
    respond s = do
        -- Get request from browser.
        input <- recv s 1024

        -- Parse URL.
        let requestUrl = (SB.split (fromIntegral $ ord ' ') input) !! 1

        case tail (SB.split (fromIntegral $ ord '/') requestUrl) of

            -- Root page.
            ("" : _) -> do
                ok s
                sendAll s $ renderHtml root

            -- Benchmark.
            (x : _) -> do
                let requestedBenchmark = SBC.unpack x
                    benchmark = M.lookup requestedBenchmark benchmarkMap
                case benchmark of
                    -- Benchmark found, run and return.
                    Just (HtmlBenchmark _ f x _) -> do
                        ok s
                        sendAll s $ f x
                    -- No benchmark found, send a 404.
                    Nothing -> notFound s

            -- Other.
            _ -> notFound s

        sClose s

    -- Construct a lookup table for benchmarks.
    benchmarkMap = let t b@(HtmlBenchmark n _ _ _) = (n, b)
                   in M.fromList $ map t benchmarks

    -- Send a 200 OK response.
    ok s = do
        _ <- send s $ "HTTP/1.1 200 OK\r\n"
                    `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
                    `mappend` "\r\n"
        return ()

    -- Send a 404 not found response.
    notFound s = do
        _ <- send s $ "HTTP/1.1 404 Not Found\r\n"
            `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
            `mappend` "\r\n"
            `mappend` "<h1>Page not found</h1>"
        return ()
