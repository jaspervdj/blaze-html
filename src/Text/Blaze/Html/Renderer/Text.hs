module Text.Blaze.Html.Renderer.Text
    ( renderHtmlBuilder
    , renderHtmlBuilderWith
    , renderHtml
    , renderHtmlWith
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Text.Blaze.Html (Html)
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Renderer.Text as R

renderHtmlBuilder :: Html -> Builder
renderHtmlBuilder = R.renderMarkupBuilder

renderHtmlBuilderWith :: (ByteString -> Text) -> Html -> Builder
renderHtmlBuilderWith = R.renderMarkupBuilderWith

renderHtml :: Html -> TL.Text
renderHtml = R.renderMarkup

renderHtmlWith :: (ByteString -> Text) -> Html -> TL.Text
renderHtmlWith = R.renderMarkupWith
