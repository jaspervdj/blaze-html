{-# LANGUAGE OverloadedStrings #-}
-- | A renderer that produces a lazy 'L.Text' value, using the Text Builder.
--
module Text.Blaze.Renderer.Text
    ( renderHtml
    , renderHtmlWith
    ) where

import Data.Monoid (mappend, mempty)
import Data.List (isInfixOf)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import Data.ByteString (ByteString)
import qualified Data.ByteString as S (isInfixOf)

import Text.Blaze.Internal
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

-- | Escape HTML entities in a text value
--
escapeHtmlEntities :: Text     -- ^ Text to escape
                   -> Builder  -- ^ Resulting text builder
escapeHtmlEntities = T.foldr escape mempty
  where
    escape :: Char -> Builder -> Builder
    escape '<'  b = B.fromText "&lt;"   `mappend` b
    escape '>'  b = B.fromText "&gt;"   `mappend` b
    escape '&'  b = B.fromText "&amp;"  `mappend` b
    escape '"'  b = B.fromText "&quot;" `mappend` b
    escape '\'' b = B.fromText "&#39;"  `mappend` b
    escape x    b = B.singleton x       `mappend` b

-- | Render a 'ChoiceString'. TODO: Optimization possibility, apply static
-- argument transformation.
--
fromChoiceString :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                 -> ChoiceString          -- ^ String to render
                 -> Builder               -- ^ Resulting builder
fromChoiceString _ (Static s)     = B.fromText $ getText s
fromChoiceString _ (String s)     = escapeHtmlEntities $ T.pack s
fromChoiceString _ (Text s)       = escapeHtmlEntities s
fromChoiceString d (ByteString s) = B.fromText $ d s
fromChoiceString d (PreEscaped x) = case x of
    String s -> B.fromText $ T.pack s
    Text   s -> B.fromText s
    s        -> fromChoiceString d s
fromChoiceString d (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromText (T.pack s)
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromText (d s)
    s            -> fromChoiceString d s
fromChoiceString d (AppendChoiceString x y) =
    fromChoiceString d x `mappend` fromChoiceString d y
fromChoiceString _ EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render some 'Html' to a Text 'Builder'.
--
renderBuilder :: (ByteString -> Text)  -- ^ Decoder for bytestrings
              -> Html                  -- ^ HTML to render
              -> Builder               -- ^ Resulting builder
renderBuilder d = go mempty 
  where
    go :: Builder -> HtmlM b -> Builder
    go attrs (Parent _ open close content) =
        B.fromText (getText open)
            `mappend` attrs
            `mappend` B.singleton '>'
            `mappend` go mempty content
            `mappend` B.fromText (getText close)
    go attrs (Leaf _ begin end) = 
        B.fromText (getText begin)
            `mappend` attrs
            `mappend` B.fromText (getText end)
    go attrs (AddAttribute _ key value h) =
        go (B.fromText (getText key)
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) h
    go attrs (AddCustomAttribute _ key value h) =
        go (fromChoiceString d key
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) h
    go _ (Content content)  = fromChoiceString d content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go _ Empty              = mempty
    {-# NOINLINE go #-}
{-# INLINE renderBuilder #-}

-- | Render HTML to a lazy Text value. If there are any ByteString's in the
-- input HTML, this function will consider them as UTF-8 encoded values and
-- decode them that way.
--
renderHtml :: Html    -- ^ HTML to render
           -> L.Text  -- ^ Resulting 'L.Text'
renderHtml = renderHtmlWith decodeUtf8
{-# INLINE renderHtml #-}

-- | Render HTML to a lazy Text value. This function allows you to specify what
-- should happen with ByteString's in the input HTML. You can decode them or
-- drop them, this depends on the application...
--
renderHtmlWith :: (ByteString -> Text)  -- ^ Decoder for ByteString's.
               -> Html                  -- ^ HTML to render
               -> L.Text                -- Resulting lazy text
renderHtmlWith d = B.toLazyText . renderBuilder d
