{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main (defaultMain,bench)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlIO
import Data.Text()
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain $ fileIO ++ streamIO

fileIO :: IO ()
fileIO = undefined

streamIO :: IO ()
streamIO = undefined
