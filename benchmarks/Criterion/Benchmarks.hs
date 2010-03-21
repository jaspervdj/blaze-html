module Criterion.Benchmarks 
    (benchmarks)
where

import Data.Monoid (mappend,mconcat)

import Text.BlazeHtml.Internal.Html
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Render.HtmlText

import Text.Html ((<<),(+++))
import qualified Text.Html as Html

import Criterion.Main (bench,Benchmark,nf)
import Criterion.Utilities

benchmarks :: [Benchmark]
benchmarks = [appendBench,appendBenchOld,tableBench
             ,nestingElements,nestingElementsOld]

-- | Benchmark for appending two simple elements.
appendBench :: Benchmark
appendBench = bench "appendBench" $ flip nf () $ \() ->
    htmlText $ applyntimes (a `mappend`) a 3
        where a = nodeElement (T.pack "p") (unescapedText string)
              string = T.pack simpleTestString

-- | Benchmark for appending two simple elements using the old library.
appendBenchOld :: Benchmark
appendBenchOld = bench "appendBenchOld" $ flip nf () $ \() ->
   Html.renderHtml $ applyntimes (a +++) a 3
       where a = Html.p << simpleTestString

-- | Benchmark for nesting elements.
nestingElements :: Benchmark
nestingElements = bench "nestingElements" $ flip nf () $ \() ->
    htmlText $ applyntimes adopt str 3
        where adopt = nodeElement (T.pack "div")
              str = unescapedText string
              string = T.pack simpleTestString

-- | Benchmark for nesting elements using the old library.
nestingElementsOld :: Benchmark
nestingElementsOld = bench "nestingElementsOld" $ flip nf () $ \() ->
   Html.renderHtml $ applyntimes adopt str 3
        where adopt = Html.thediv
              str = Html.toHtml simpleTestString

-- | Just a 1KB text string.
simpleTestString :: String
simpleTestString = replicate (1024*1024 :: Int) 'a'


tableBench :: Benchmark
tableBench = bench "table benchmark" $ flip nf () $ \() ->
    htmlText $ applyntimes (a `mappend`) a 1000
        where a = nodeElement (T.pack "tr") b
              b = mconcat $ replicate 10 $ nodeElement (T.pack "td") (unescapedText string)
              string = T.pack "winter"
