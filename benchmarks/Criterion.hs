{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main (defaultMain,bench)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlIO
import Data.Text()
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain [
       -- ,bench "fib 30" $ whnf fib 30
       bench "addUnescapedAttributes [(\"class\",\"demo\")]" $ renderHtmlIO T.putStr $ addUnescapedAttributes [("class","demo")] $ renderLeafElement "p"
       ]
