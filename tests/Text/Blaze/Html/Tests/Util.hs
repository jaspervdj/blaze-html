-- | Utility functions for the blaze tests
--
module Text.Blaze.Html.Tests.Util
    ( renderUsingString
    , renderUsingText
    , renderUsingUtf8
    ) where

import Text.Blaze.Html5 hiding (map)
import qualified Data.ByteString.Lazy as LB
import qualified Text.Blaze.Renderer.Utf8 as Utf8 (renderHtml)
import qualified Text.Blaze.Renderer.Text as Text (renderHtml)
import qualified Text.Blaze.Renderer.String as String (renderHtml)
import Blaze.ByteString.Builder as B (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 as B (fromString)
import Data.Text.Lazy.Encoding (encodeUtf8)

-- | Render HTML to an UTF-8 encoded ByteString using the String renderer
--
renderUsingString :: Html -> LB.ByteString
renderUsingString = toLazyByteString . fromString . String.renderHtml

-- | Render HTML to an UTF-8 encoded ByteString using the Text renderer
--
renderUsingText :: Html -> LB.ByteString
renderUsingText = encodeUtf8 . Text.renderHtml

-- | Render HTML to an UTF-8 encoded ByteString using the Utf8 renderer
--
renderUsingUtf8 :: Html -> LB.ByteString
renderUsingUtf8 = Utf8.renderHtml
