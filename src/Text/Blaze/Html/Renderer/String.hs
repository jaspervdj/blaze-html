module Text.Blaze.Html.Renderer.String
    ( renderHtml
    ) where

import Text.Blaze.Html (Html)
import Text.Blaze.Renderer.String (renderMarkup)

renderHtml :: Html -> String
renderHtml = renderMarkup
