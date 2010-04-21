-- | Core exposed functions.
module Text.Blaze
    ( 
      -- * Important types.
      Html

      -- * Converting values to HTML.
    , text
    , rawByteString
    , showHtml

      -- * Setting attributes
    , (!)

      -- * Rendering HTML.
    , renderHtml
    ) where

import Data.Monoid (mappend, mempty)

import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)

import Text.Blaze.Internal.Utf8Builder
import Text.Blaze.Internal.Html

-- | Render escaped text.
--
text :: Text -- ^ Text to render.
     -> Html -- ^ Resulting HTML fragment.
text = HtmlM . const . fromHtmlText
{-# INLINE text #-}

-- | Render a raw 'ByteString'. This function will not do any HTML escaping,
-- so be careful with it.
--
rawByteString :: ByteString -- ^ Raw 'ByteString' to render.
              -> Html       -- ^ Resulting HTML fragment.
rawByteString = HtmlM . const . fromRawByteString
{-# INLINE rawByteString #-}

-- | Create a HTML snippet from a 'Show'able type.
--
showHtml :: Show a => a -> Html
showHtml = HtmlM . const . fromHtmlString . show
{-# INLINE showHtml #-}

-- | /O(n)./ Render the HTML fragment to lazy 'L.ByteString'.
--
renderHtml :: Html -> L.ByteString
renderHtml = toLazyByteString . flip runHtml mempty
