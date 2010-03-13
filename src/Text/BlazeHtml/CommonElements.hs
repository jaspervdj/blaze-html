module Text.BlazeHtml.CommonElements
    ( a
    , div_
    , em
    , h1
    , img
    , p
    , text
    ) where

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.BlazeHtml

a :: Text -> Html -> Html
a href html = toHtml $ HtmlElement (T.pack "a")
                                   (M.singleton (T.pack "href") href)
                                   html

div_ :: Html -> Html
div_ = toHtml . HtmlElement (T.pack "div") M.empty

em :: Html -> Html
em = toHtml . HtmlElement (T.pack "em") M.empty

h1 :: Text -> Html
h1 = toHtml . HtmlElement (T.pack "h1") M.empty . text

img :: Text -> Text -> Html
img src alt = toHtml $ HtmlLeafElement (T.pack "img") $ M.fromList
    [ (T.pack "src", src)
    , (T.pack "alt", alt)
    ]

p :: Html -> Html
p = toHtml . HtmlElement (T.pack "p") M.empty

text :: Text -> Html
text = toHtml . HtmlTextElement
