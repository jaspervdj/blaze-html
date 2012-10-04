module Text.Blaze.Html.Renderer.Utf8
    ( renderHtmlBuilder
    , renderHtml
    ) where

import Data.ByteString.Builder (Builder)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BL
import qualified Text.Blaze.Renderer.Utf8 as R

renderHtmlBuilder :: Html -> Builder
renderHtmlBuilder = R.renderMarkupBuilder

renderHtml :: Html -> BL.ByteString
renderHtml = R.renderMarkup
