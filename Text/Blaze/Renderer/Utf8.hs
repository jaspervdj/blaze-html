{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.Utf8
    ( renderHtml
    ) where

import Data.Monoid (mappend, mempty)
import Data.List (isInfixOf)

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T (isInfixOf)
import qualified Data.ByteString as S (isInfixOf)

import Text.Blaze.Internal
import Text.Blaze.Builder.Core (Builder)
import qualified Text.Blaze.Builder.Core as B
import qualified Text.Blaze.Builder.Utf8 as B
import qualified Text.Blaze.Builder.Html as B

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder       -- ^ Resulting builder
fromChoiceString (Static s)     = B.fromByteString $ getUtf8ByteString s
fromChoiceString (String s)     = B.fromHtmlEscapedString s
fromChoiceString (Text s)       = B.fromHtmlEscapedText s
fromChoiceString (ByteString s) = B.fromByteString s
fromChoiceString (PreEscaped x) = case x of
    String s -> B.fromString s
    Text   s -> B.fromText s
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromString s
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromByteString s
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x `mappend` fromChoiceString y
fromChoiceString EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render some 'Html' to a 'Builder'.
--
renderBuilder :: HtmlM a  -- ^ HTML to render
              -> Builder  -- ^ Resulting builder
renderBuilder = go mempty 
  where
    go :: Builder -> HtmlM b -> Builder
    go attrs (Parent open close content) =
        B.fromByteString (getUtf8ByteString open)
            `mappend` attrs
            `mappend` B.fromChar '>'
            `mappend` go mempty content
            `mappend` B.fromByteString (getUtf8ByteString close)
    go attrs (Leaf begin end) = 
        B.fromByteString (getUtf8ByteString begin)
            `mappend` attrs
            `mappend` B.fromByteString (getUtf8ByteString end)
    go attrs (AddAttribute key value h) =
        go (B.fromByteString (getUtf8ByteString key)
            `mappend` fromChoiceString value
            `mappend` B.fromChar '"'
            `mappend` attrs) h
    go attrs (AddCustomAttribute key value h) =
        go (fromChoiceString key
            `mappend` fromChoiceString value
            `mappend` B.fromChar '"'
            `mappend` attrs) h
    go _ (Content content)  = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go _ Empty              = mempty
    {-# NOINLINE go #-}
{-# INLINE renderBuilder #-}

-- | Render HTML to a lazy UTF-8 encoded 'L.ByteString.'
--
renderHtml :: HtmlM a       -- ^ HTML to render
           -> L.ByteString  -- ^ Resulting 'L.ByteString'
renderHtml = B.toLazyByteString . renderBuilder
{-# INLINE renderHtml #-}
