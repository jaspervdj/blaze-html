Benchmark server: all benchmarks from 'Utf8Html' can be requested through this
server via URL's. At the same time, the server shows some information about
these benchmarks at the root URL.

Example:

    GET /manyAttributes HTTP/1.1

Will give you the @manyAttributes@ benchmark.

Example:

    GET / HTTP/1.1

Will give you a page with some information and a benchmark listing As always in
a literate Haskell file, feel free to skip the imports.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where

> import Prelude hiding (putStrLn)

> import Control.Concurrent (forkIO)
> import Data.Monoid (mappend, mconcat)
> import Control.Applicative ((<$>))
> import Control.Monad (forever, forM_)
> import Network.Socket (accept, sClose)
> import Network (listenOn, PortID (PortNumber))
> import System (getArgs)
> import Data.Char (ord, toLower)
> import Data.Map (Map)
> import qualified Data.Map as M

> import Network.Socket.ByteString (recv, send)
> import Network.Socket.ByteString.Lazy (sendAll)
> import qualified Data.ByteString as SB
> import qualified Data.ByteString.Char8 as SBC
> import qualified Data.ByteString.Lazy as LB

> import Utf8Html (HtmlBenchmark (..), benchmarks)
> import Text.Blaze.Html5 hiding (map, head)
> import qualified Text.Blaze.Html5 as H
> import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

Our first template is the root page. It's a static template, so it takes no
parameters.

> root :: Html
> root = html $ do
>     H.head $ do
>         title "BlazeHtml benchmarks"
>     body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
>         h1 "Benchmarks"
>         p $ do
>             "This is a server showing BlazeHtml benchmarks, written using"
>             " BlazeHtml. You can check out this benchmarks here, but it is"
>             " probably more interesting to measure them using a tool like"
>             " " >> code "ab" >> " or " >> code "httperf" >> "."

Now, the interesting part comes. `benchmarks` is a list of `HtmlBenchmark`'s,
imported from the `Utf8Html` module, which contains a number of benchmarks for
BlazeHtml. Here, we *loop* over these benchmarks and render them using the
`benchmark` template (defined below).

>         forM_ benchmarks benchmark

Alternatively, one could have written `mconcat $ map benchmark benchmarks`.

And so, we arrive at our second benchmark. It simply pattern matches on a
`HtmlBenchmark` (which only has one constructor) and generates some HTML
describing this benchmark.

> benchmark :: HtmlBenchmark -> Html
> benchmark (HtmlBenchmark name _ _ description) = do
>     h2 $ string name
>     p $ description
>     p $ "URL: " >> code ("/" >> string name)
>     p $ a ! href (stringValue name) $ "Go to benchmark."

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

We split the URL on '/' characters and drop the first element of the resulting
list -- this element contains no information, since the URL will always start
with a '/'.

>         case tail (SB.split (fromIntegral $ ord '/') requestUrl) of

If the result of this split is empty, it means we have accessed the root URL. In
that case, we send the root template back. The `ok s` sends a `200 OK` response,
letting the browser know all is fine.

>             [""] -> do
>                 ok s
>                 sendAll s $ renderHtml root

We do a very broad pattern match now, taking the first part of the URL. This is
supposed to be the name of a benchmark.

>             (x : _) -> do

We convert the benchmark name to lowercase, and then look it up in a lookup
table which maps benchmark names to actual benchmarks.

>                 let requestedBenchmark = map toLower $ SBC.unpack x
>                     benchmark = M.lookup requestedBenchmark benchmarkMap

If all goes well, we found a benchmark. In that case, we can run and send it.

>                 case benchmark of
>                     Just (HtmlBenchmark _ f x _) -> do
>                         ok s
>                         sendAll s $ renderHtml $ f x

If the benchmark is not found, we give a `404` error back.

>                     Nothing -> notFound s

If our earlier pattern match failed (which really should not happen) we send a
`404`, too.

>             _ -> notFound s

After this, we can safely close the connection.

>         sClose s

Now, some auxiliary functions follow. The first one is the function that build
the lookup table of benchmarks. Note that we store the key as lowercase, so our
URL's we be case-independent.

>     benchmarkMap = let t b@(HtmlBenchmark n _ _ _) = (map toLower n, b)
>                    in M.fromList $ map t benchmarks

Now, we have two functions which send a response header back to the browser,
nothing special going on here.

>     ok s = do
>         _ <- send s $ "HTTP/1.1 200 OK\r\n"
>                     `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
>                     `mappend` "\r\n"
>         return ()
> 
>     notFound s = do
>         _ <- send s $ "HTTP/1.1 404 Not Found\r\n"
>             `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
>             `mappend` "\r\n"
>             `mappend` "<h1>Page not found</h1>"
>         return ()

Note that while we directly used sockets here, this is probably *not* the way
you want to go if you are writing a serious web application. In that case, you
should use BlazeHtml behind some kind of web framework (unless you're really
masochistic).
