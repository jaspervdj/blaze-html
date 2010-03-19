{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main (defaultMain,bench,whnf)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlIO
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = defaultMain [
       -- ,bench "fib 30" $ whnf fib 30
       bench "addAttributes [(\"class\",\"demo\")]" $ renderHtmlIO T.putStr $ addAttributes [("class","demo")] $ renderLeafElement "p"
       ]
