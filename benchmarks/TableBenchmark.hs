module TableBenchmark where

import Criterion
import Control.Concurrent (forkIO,killThread)
import Control.Monad (forever)
import Data.Monoid (mappend,mconcat)
import Network (PortID(PortNumber),Socket)
import qualified Network as N
import System.IO
import System.Console.GetOpt
import System.Directory (getCurrentDirectory,removeFile)
import System.Environment

import Criterion.Main (defaultMain,bench,Benchmark,whnf)

import Text.Html ((<<),(+++))
import qualified Text.Html as Html

import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Render.HtmlText


tableBench :: Text
tableBench = 
    htmlText $ applyntimes (a `mappend`) a 2
        where a = nodeElement (T.pack "tr") b
              b = mconcat $ replicate 10 $ nodeElement (T.pack "th") (unescapedText string)
              string = T.pack "winter"

fileIO2 :: Handle -> [Benchmark]
fileIO2 h = map (bench "fileIO tableBench" . fileTest h) benches where
    benches = [tableBench]


runBenchmarks :: IO ()
runBenchmarks = do
  N.withSocketsDo   $
    withSocketTests $ \socketTests ->
    withFileTests   $ \fileTests   -> do
      defaultMain $ socketTests ++ fileTests
