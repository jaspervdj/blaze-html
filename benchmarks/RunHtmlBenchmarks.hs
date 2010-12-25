-- | This is a module which runs the 'HtmlBenchmarks' module using the different
-- renderers available.
--
module RunHtmlBenchmarks where

import Criterion.Main
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import qualified Text.Blaze.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Renderer.String as String
import qualified Text.Blaze.Renderer.Text as Text

import HtmlBenchmarks (HtmlBenchmark (..), benchmarks)

-- | Function to run the benchmarks using criterion
--
main :: IO ()
main = defaultMain $ concatMap benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) =
        [ bench (name ++ " (Utf8)")   $ nf (LB.length .  Utf8.renderHtml . f) x
        , bench (name ++ " (String)") $ nf (String.renderHtml . f) x
        , bench (name ++ " (Text)")   $ nf (LT.length . Text.renderHtml . f) x
        ]
