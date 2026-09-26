module Text.Blaze.Html.Renderer.Utf8
    ( renderHtmlBuilder
    , renderHtml
    , renderHtmlToByteStringIO
    ) where

import Blaze.ByteString.Builder (Builder)
import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BL
import qualified Text.Blaze.Renderer.Utf8 as R

renderHtmlBuilder :: Html -> Builder
renderHtmlBuilder = R.renderMarkupBuilder

renderHtml :: Html -> BL.ByteString
renderHtml = R.renderMarkup

-- | @renderHtmlToByteStringIO f h@ allocates a 'ByteString' /once/; renders
-- chunks of 'Html' content to it; calls @f@ after every render. This way we
-- and have a very fast loop.
--
-- This also means using @writeFile "example.html"@ for @f@ is /wrong/, as
-- we would be /replacing/ the file for every chunk.
--
-- Example of correct usage:
--
-- @
--     IO.withFile ("z.utf8." <> path) IO.WriteMode $ \h ->
--       renderHtmlToByteStringIO (B.hPutStr h) html
-- @
renderHtmlToByteStringIO :: (ByteString -> IO ()) -> Html -> IO ()
renderHtmlToByteStringIO = R.renderMarkupToByteStringIO
