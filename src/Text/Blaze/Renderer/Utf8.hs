{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.Utf8
    ( renderHtml
    ) where

import Data.Monoid (mappend, mempty)

import qualified Data.ByteString.Lazy as L

import Text.Blaze.Internal
import Text.Blaze.Internal.Builder.Core (Builder)
import qualified Text.Blaze.Internal.Builder.Core as B
import qualified Text.Blaze.Internal.Builder.Utf8 as B
import qualified Text.Blaze.Internal.Builder.Html as B

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder       -- ^ Resulting builder
fromChoiceString (Static     s) = B.copyByteString $ getUtf8ByteString s
fromChoiceString (String     s) = B.fromHtmlEscapedString s
fromChoiceString (Text       s) = B.fromHtmlEscapedText s
fromChoiceString (ByteString s) = B.copyByteString s
fromChoiceString (PreEscaped s) = case s of
    String s' -> B.fromString s'
    Text   s' -> B.fromText s'
    x         -> fromChoiceString x
{-# INLINE fromChoiceString #-}

-- | Render some 'Html' to a 'Builder'.
--
renderBuilder :: Html a   -- ^ HTML to render
              -> Builder  -- ^ Resulting builder
renderBuilder = go mempty 
  where
    go :: Builder -> Html b -> Builder
    go attrs (Parent open close content) =
        B.copyByteString (getUtf8ByteString open)
            `mappend` attrs
            `mappend` B.fromChar '>'
            `mappend` go mempty content
            `mappend` B.copyByteString (getUtf8ByteString close)
    go attrs (Leaf begin end) = 
        B.copyByteString (getUtf8ByteString begin)
            `mappend` attrs
            `mappend` B.copyByteString (getUtf8ByteString end)
    go attrs (Open begin end) = 
        B.copyByteString (getUtf8ByteString begin)
            `mappend` attrs
            `mappend` B.copyByteString (getUtf8ByteString end)
    go attrs (AddAttribute key value h) =
        go (attrs `mappend` B.copyByteString (getUtf8ByteString key)
            `mappend` fromChoiceString value
            `mappend` B.fromChar '"') h
    go _ (Content content)  = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go _ Empty              = mempty
    {-# NOINLINE go #-}

-- | Render HTML to a lazy UTF-8 encoded 'L.ByteString.'
--
renderHtml :: Html a
           -> L.ByteString
renderHtml = B.toLazyByteString . renderBuilder
{-# INLINE renderHtml #-}
