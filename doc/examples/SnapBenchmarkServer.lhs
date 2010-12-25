This is a more "complicated" example of BlazeHtml running behind the [Snap]
Haskell web framework.

[Snap]: http://snapframework.com

> {-# LANGUAGE OverloadedStrings #-}
> module SnapBenchmarkServer where

> import Data.Maybe (fromMaybe)
> import Data.Char (toLower)
> import Control.Applicative ((<$>))
> import qualified Data.Map as M

> import Snap.Http.Server
> import Snap.Types
> import qualified Data.ByteString.Char8 as SBC

We re-use most of the `BenchmarkServer`, and the `blazeTemplate` function from
the simple `SnapFramework` example as well.

> import BenchmarkServer hiding (main)
> import HtmlBenchmarks (HtmlBenchmark (..))
> import SnapFramework (blazeTemplate)

We now present "Handlers" for our templates: these are values of the type
`Snap ()`, and present a response. The first handler simply renders the root
page:

> rootHandler :: Snap ()
> rootHandler = blazeTemplate root

The benchmark handler looks up the requested benchmark in the `benchmarkMap`,
runs it if it is found or sends a 404 (`notFound`) if the benchmark is not
found.

> benchmarkHandler :: Snap ()
> benchmarkHandler = do
>      b <- fromMaybe "" <$> getParam "b"
>      case M.lookup (map toLower $ SBC.unpack b) benchmarkMap of
>          Just (HtmlBenchmark _ f x _) -> blazeTemplate $ f x
>          Nothing                      -> notFound

This is a simple handler which sends a 404. I really feel there should be a
standard function in Snap which does this. If there is, contact me!

> notFound :: Snap ()
> notFound = do
>     modifyResponse $ setResponseStatus 404 "Not Found"
>     writeBS "404 error"
>     r <- getResponse
>     finishWith r

Then, we present the routes for our benchmark server. We have the root page at
the top, and then we can access the benchmarks by name.

> site :: Snap ()
> site = route [ ("",    rootHandler)
>              , ("/:b", benchmarkHandler)
>              ]

The main function is simply the same as in the `SnapFramework` example.

> main :: IO ()
> main = httpServe defaultConfig site
