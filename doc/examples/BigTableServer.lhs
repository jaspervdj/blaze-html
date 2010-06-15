This is a simple benchmark containing some IO. This program runs as a HTTP
server on a given port. It responds by spamming big tables in the right size.
The request protocol for clients is:

    GET /rows/columns HTTP/1.1

For example,

    GET /1000/10 HTTP/1.1

The server then responds with an HTML table with the requested dimensions. If
the requested URL is invalid, the server will send a 1000x10 table.

Apart from being an example, this is also a performance test. Hence, we are
using sockets directly instead of running behind, say, CGI or a web framework.
Other examples illustrate how to do that.

As always in a literate Haskell file, feel free to skip the imports.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where

> import Prelude hiding (putStrLn)

> import Control.Concurrent (forkIO)
> import Data.Monoid (mappend, mconcat)
> import Control.Applicative ((<$>))
> import Control.Monad (forever)
> import Network.Socket (accept, sClose)
> import Network (listenOn, PortID (PortNumber))
> import System (getArgs)
> import Data.Char (ord)

> import Network.Socket.ByteString (recv, send)
> import Network.Socket.ByteString.Lazy (sendAll)
> import qualified Data.ByteString as SB
> import qualified Data.ByteString.Char8 as SBC
> import qualified Data.ByteString.Lazy as LB

> import Text.Blaze.Html5 hiding (map, head)

We only have one simple template in this benchmark: a template that takes a
matrix as input, and produces an HTML table containing this matrix.

> bigTable :: [[Int]]  -- ^ Matrix.
>          -> Html a   -- ^ Result.
> bigTable t = table $ mconcat $ map row t
>   where
>     row r = tr $ mconcat $ map (td . string . show) r

This is the main function that runs the server. It takes one command line
argument: the port it should listen on.

> main :: IO ()
> main = do
>     port <- PortNumber . fromIntegral . read . head <$> getArgs
>     socket <- listenOn port

It forks for every incoming connection -- this is the standard control flow in
most Haskell servers.

>     forever $ do
>         (s, _) <- accept socket
>         forkIO (respond s)
>   where

Okay, now we get to the fun part: parsing the request and building the response.

>     respond s = do

We take 1024 bytes from the socket -- this should be plenty. The HTTP request
will look something like `GET /url HTTP/1.1`. We are only interested in the url,
so we split the input on spaces and take the second part (`!!` is zero-based).

>         input <- recv s 1024
>         let requestUrl = (SB.split (fromIntegral $ ord ' ') input) !! 1

Since we're always going to send a response, we let the browser know
everything is fine here.

>         _ <- send s $ "HTTP/1.1 200 OK\r\n"
>             `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
>             `mappend` "\r\n"

So, now we're going to do a bit of string splitting and parsing to get our
`width` and `height` from the `/height/width` URL.

>         let (h, w) = case SB.split (fromIntegral $ ord '/') requestUrl of

If we have a valid URL, we parse the height and width. Otherwise, we just take a
fixed height and width.

>                         (_ : h' : w' : _) ->
>                             (read $ SBC.unpack h', read $ SBC.unpack w')
>                         _ -> (1000, 10)

Building the matrix is now fairly straightforward.

>             matrix = replicate h [1 .. w]


We then render it and use the `sendAll` function to send our resulting lazy
`ByteString`. Afterwards, we can close the socket.

>         sendAll s $ renderHtml $ bigTable matrix
>         sClose s
