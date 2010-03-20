{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main (defaultMain,bench)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlIO
import Data.Text
import System.IO
import Text.BlazeHtml.Render.HtmlText
import qualified Data.Text.IO as T

main :: IO ()
main = do
  h <- openFile "./testfile.txt" WriteMode
  defaultMain $ fileIO h
  hClose h

fileIO h = [bench "file io test" $ fileTest h simpleText]

fileTest :: Handle -> Text -> IO ()
fileTest h t = T.hPutStr h t

simpleText = renderHtmlText (addUnescapedAttributes [("class","demo")] $ renderLeafElement "p")

