{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
-- | Internal HTML stuff.
module Text.Blaze.Internal.Html
    ( 
      -- * Types.
      HtmlM (..)
    , Html

    , tag

    , Attribute
    , attribute
    , (!)
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

newtype Attribute = Attribute (Html -> Html)

-- | Add an attribute to the current element.
--
attribute :: ByteString -> Text -> Attribute
attribute key value = Attribute $ \(HtmlM h) -> HtmlM $ \attrs ->
    h $ attrs `mappend` (fromRawAscii7Char ' '
              `mappend` (fromRawByteString key
              `mappend` (fromRawByteString "=\""
              `mappend` (fromHtmlText value
              `mappend` (fromRawAscii7Char '"')))))
{-# INLINE attribute #-}

class Attributable h where
    -- | Apply an attribute on an element.
    --
    (!) :: h -> Attribute -> h

instance Attributable Html where
    h ! (Attribute a) = a h
    {-# INLINE (!) #-}
    {-# SPECIALIZE (!) :: Html -> Attribute -> Html #-}

instance Attributable (Html -> Html) where
    f ! (Attribute a) = \h -> a (f h)
    {-# INLINE (!) #-}
    {-# SPECIALIZE (!) :: (Html -> Html) -> Attribute -> (Html -> Html) #-}
