{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.HtmlElements
    ( em
    , h1
    , img
    , p
    , text
    ) where

import Data.Text (Text)

import Text.BlazeHtml

em :: Html () -> Html ()
em = renderElement "em"

h1 :: Text -> Html ()
h1 = renderElement "h1" . renderText

img :: Text -> Text -> Html ()
img src alt = renderLeafElement "img" ! [("src", src), ("alt", alt)]

p :: Html () -> Html ()
p = renderElement "p"

text :: Text -> Html ()
text = renderText
