module Text.Blaze.Html
    ( module Text.Blaze
    , Html
    , toHtml
    ) where

import Text.Blaze

type Html = Markup

toHtml :: ToMarkup a => a -> Markup 
toHtml = toMarkup
