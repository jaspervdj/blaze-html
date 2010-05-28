{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | Core exposed functions.
module Text.Blaze
    (
      -- * Important types.
      Html
    , Attribute
    , AttributeValue

      -- * Creating custom tags and attributes.
    , parent
    , leaf
    , attribute
    , open

      -- * Converting values to HTML.
    , text
    , preEscapedText
    , string
    , preEscapedString

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , stringValue
    , preEscapedStringValue

      -- * Setting attributes
    , (!)

      -- * Rendering HTML.
    , renderHtml
    ) where

import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import GHC.Exts (IsString (..))

import Text.Blaze.Internal.Utf8Builder (Utf8Builder)
import qualified Text.Blaze.Internal.Utf8Builder as B

-- | The core HTML datatype.
--
newtype HtmlM a = HtmlM
    { -- | Function to extract the 'Builder'.
      runHtml :: Utf8Builder -> Utf8Builder
    }

-- | Simplification of the 'HtmlM' type.
--
type Html = HtmlM ()

-- | Type for an attribute.
--
newtype Attribute = Attribute (Html -> Html)

-- | The type for an attribute value.
--
newtype AttributeValue = AttributeValue { attributeValue :: Utf8Builder }

instance Monoid (HtmlM a) where
    mempty = HtmlM $ \_ -> mempty
    {-# INLINE mempty #-}
    --SM: Note for the benchmarks: We should test which multi-`mappend`
    --versions are faster: right or left-associative ones. Then we can register
    --a rewrite rule taking care of that. I actually guess that this may be one
    --of the reasons accounting for the speed differences between monadic
    --syntax and monoid syntax: the rewrite rules for monadic syntax bring the
    --`>>=` into the better form which results in a better form for `mappend`.
    (HtmlM h1) `mappend` (HtmlM h2) = HtmlM $ \attrs ->
        h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = HtmlM $ \attrs ->
        foldr mappend mempty $ map (flip runHtml attrs) hs
    {-# INLINE mconcat #-}

instance Monad HtmlM where
    return _ = mempty
    {-# INLINE return #-}
    (HtmlM h1) >> (HtmlM h2) = HtmlM $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}

instance IsString (HtmlM ()) where
    fromString = string
    {-# INLINE fromString #-}

instance IsString AttributeValue where
    fromString = stringValue
    {-# INLINE fromString #-}

-- | Create an HTML parent element.
--
parent :: Text  -- ^ HTML element tag.
       -> Html  -- ^ Inner HTML, to place in this element.
       -> Html  -- ^ Resulting HTML.
parent tag = \inner -> HtmlM $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.fromPreEscapedChar '>'
      `mappend` runHtml inner mempty
      `mappend` end
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromPreEscapedText $ "<" `mappend` tag
    end :: Utf8Builder
    end = B.optimizePiece $ B.fromPreEscapedText $ "</" `mappend` tag
                                                        `mappend` ">"
{-# INLINE parent #-}

-- | Create an HTML leaf element.
--
leaf :: Text  -- ^ HTML element tag.
     -> Html  -- ^ Resulting HTML.
leaf tag = HtmlM $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.unsafeFromByteString " />"
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromPreEscapedText $ "<" `mappend` tag
{-# INLINE leaf #-}

-- | Produce an open tag. This can be used for open tags in HTML 4.01, like
-- for example @<br>@.
--
open :: Text  -- ^ Tag for the open HTML element.
     -> Html  -- ^ Resulting HTML.
open tag = HtmlM $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.unsafeFromByteString ">"
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromPreEscapedText $ "<" `mappend` tag
{-# INLINE open #-}

-- | Create an HTML attribute.
--
attribute :: Text            -- ^ Key for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute key value = Attribute $ \(HtmlM h) -> HtmlM $ \attrs ->
    h $ attrs `mappend` begin
              `mappend` attributeValue value
              `mappend` B.fromPreEscapedChar '"'
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromPreEscapedText $ " " `mappend` key
                                                         `mappend` "=\""
{-# INLINE attribute #-}

class Attributable h where
    -- | Apply an attribute on an element.
    --
    (!) :: h -> Attribute -> h

instance Attributable (HtmlM ()) where
    h ! (Attribute a) = a h
    {-# INLINE (!) #-}
    {-# SPECIALIZE (!) :: Html -> Attribute -> Html #-}

instance Attributable (HtmlM () -> HtmlM ()) where
    f ! (Attribute a) = \h -> a (f h)
    {-# INLINE (!) #-}
    {-# SPECIALIZE (!) :: (Html -> Html) -> Attribute -> (Html -> Html) #-}

-- | Render text. This is the preferred way of converting string
-- datatypes to HTML.
--
text :: Text  -- ^ Text to render.
     -> Html  -- ^ Resulting HTML fragment.
text = HtmlM . const . B.fromText
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Text  -- ^ Text to insert.
               -> Html  -- Resulting HTML fragment.
preEscapedText = HtmlM . const . B.fromPreEscapedText
{-# INLINE preEscapedText #-}

-- | Create a HTML snippet from a 'String'.
--
string :: String -> Html
string = HtmlM . const . B.fromString
{-# INLINE string #-}

-- Why not provide a 'fromShow :: Show a => a -> Html' method?

-- | Create a HTML snippet from a 'String' without escaping
--
preEscapedString :: String -> Html
preEscapedString = HtmlM . const . B.fromPreEscapedString
{-# INLINE preEscapedString #-}

-- | Render an attrbitute value from 'Text'.
--
textValue :: Text            -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . B.fromText
{-# INLINE textValue #-}

-- | Render an attribute value from 'Text' without escaping.
--
preEscapedTextValue :: Text            -- ^ Text to insert.
                    -> AttributeValue  -- Resulting HTML fragment.
preEscapedTextValue = AttributeValue . B.fromPreEscapedText
{-# INLINE preEscapedTextValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . B.fromString
{-# INLINE stringValue #-}

-- | Create an attribute value from a 'String' without escaping.
--
preEscapedStringValue :: String -> AttributeValue
preEscapedStringValue = AttributeValue . B.fromPreEscapedString
{-# INLINE preEscapedStringValue #-}

-- | /O(n)./ Render the HTML fragment to lazy 'L.ByteString'.
--
renderHtml :: Html -> L.ByteString
renderHtml = B.toLazyByteString . flip runHtml mempty
{-# INLINE renderHtml #-}
