module Text.Blaze.Html
    ( module Text.Blaze
    , Html
    , toHtml
    , preEscapedToHtml
    ) where

import Text.Blaze

type Html = Markup

toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup

preEscapedToHtml :: ToMarkup a => a -> Html
preEscapedToHtml = preEscapedToMarkup
