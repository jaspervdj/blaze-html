module TableBenchmark where

import Criterion
import Data.Monoid (mappend,mconcat)
import qualified Network as N
import System.IO
import System.Directory (getCurrentDirectory,removeFile)

import Criterion.Main (defaultMain,bench,Benchmark,whnf)

import Text.Html ((<<),(+++))
import qualified Text.Html as Html

import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Render.HtmlText


tableBench :: Text
tableBench = 
    htmlText $ applyntimes (a `mappend`) a 1000
        where a = nodeElement (T.pack "tr") b
              b = mconcat $ replicate 10 $ nodeElement (T.pack "td") (unescapedText string)
              string = T.pack "winter"

fileIO2 :: Handle -> [Benchmark]
fileIO2 h = map (bench "fileIO tableBench" . fileTest h) benches where
    benches = [tableBench]

runBenchmarks2 :: IO ()
runBenchmarks2 = do
  N.withSocketsDo   $
    withFileTests2   $ \fileTests   -> do
      defaultMain $ fileTests


withFileTests2 :: ([Benchmark] -> IO ()) -> IO ()
withFileTests2 m = do
  dir <- getCurrentDirectory
  (n,fh) <- openTempFile dir "testfile"
  m $ fileIO2 fh
  hClose fh
  removeFile n
