{-# LANGUAGE OverloadedStrings #-}
-- | Internal HTML stuff.
module Text.Blaze.Internal.Html
    ( 
      -- * Types.
      HtmlM (..)
    , Html

    , tag
    , addAttribute
    ) where

import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.Binary.Builder (Builder)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

import Text.Blaze.Internal.Utf8Builder

-- | The core HTML datatype.
--
newtype HtmlM a = HtmlM
    { -- | Function to extract the 'Builder'.
      --
      runHtml :: Builder -> Builder
    }

-- | Simplification of the 'HtmlM' type.
--
type Html = HtmlM ()

-- | Create an HTML tag.
--
tag :: ByteString -> ByteString -> Html -> Html
tag begin end = \inner -> HtmlM $ \attrs ->
    fromRawByteString begin
      `mappend` attrs
      `mappend` fromRawAscii7Char '>'
      `mappend` runHtml inner mempty
      `mappend` fromRawByteString end
{-# INLINE tag #-}

-- | Add an attribute to the current element.
--
addAttribute :: ByteString -> Text -> Html -> Html
addAttribute key value (HtmlM h) = HtmlM $ \attrs ->
    h attrs `mappend` (fromRawAscii7Char ' '
            `mappend` (fromRawByteString key
            `mappend` (fromRawByteString "=\""
            `mappend` (fromHtmlText value
            `mappend` (fromRawAscii7Char '"')))))
{-# INLINE addAttribute #-}

instance Monoid (HtmlM a) where
    mempty = HtmlM $ \_ -> mempty
    {-# INLINE mempty #-}
    (HtmlM h1) `mappend` (HtmlM h2) = HtmlM $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = HtmlM $ \attrs ->
        foldr (\h k -> runHtml h attrs `mappend` k) mempty hs
    {-# INLINE mconcat #-}

instance Monad HtmlM where
    return a = mempty
    {-# INLINE return #-}
    (HtmlM h1) >> (HtmlM h2) = HtmlM $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}
