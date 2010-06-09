{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances #-}
-- | Core exposed functions.
module Text.Blaze
    (
      -- * Important types.
      Html
    , Tag
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
    , showHtml
    , preEscapedShowHtml

      -- * Inserting literal 'ByteString's.
    , unsafeByteString

      -- * Converting values to tags.
    , textTag
    , stringTag

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
import qualified Text.Blaze.Internal.Utf8BuilderHtml as B

-- | The core HTML datatype.
--
newtype Html a = Html
    { -- | Function to extract the 'Builder'.
      unHtml :: Utf8Builder -> Utf8Builder
    }

-- | Type for an HTML tag.
--
newtype Tag = Tag { unTag :: Utf8Builder }
    deriving (Monoid)

-- | Type for an attribute.
--
newtype Attribute = Attribute (forall a. Html a -> Html a)

-- | The type for an attribute value.
--
newtype AttributeValue = AttributeValue { attributeValue :: Utf8Builder }
    deriving (Monoid)

instance Monoid (Html a) where
    mempty = Html $ \_ -> mempty
    {-# INLINE mempty #-}
    --SM: Note for the benchmarks: We should test which multi-`mappend`
    --versions are faster: right or left-associative ones. Then we can register
    --a rewrite rule taking care of that. I actually guess that this may be one
    --of the reasons accounting for the speed differences between monadic
    --syntax and monoid syntax: the rewrite rules for monadic syntax bring the
    --`>>=` into the better form which results in a better form for `mappend`.
    (Html h1) `mappend` (Html h2) = Html $ \attrs ->
        h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = Html $ \attrs ->
        foldr mappend mempty $ map (flip unHtml attrs) hs
    {-# INLINE mconcat #-}

instance Monad Html where
    return _ = mempty
    {-# INLINE return #-}
    (Html h1) >> (Html h2) = Html $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}

instance IsString (Html a) where
    fromString = string
    {-# INLINE fromString #-}

instance IsString Tag where
    fromString = stringTag
    {-# INLINE fromString #-}

instance IsString AttributeValue where
    fromString = stringValue
    {-# INLINE fromString #-}

-- | Create an HTML parent element.
--
parent :: Tag     -- ^ HTML element tag.
       -> Html a  -- ^ Inner HTML, to place in this element.
       -> Html b  -- ^ Resulting HTML.
parent tag = \inner -> Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.fromChar '>'
      `mappend` unHtml inner mempty
      `mappend` end
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromChar '<' `mappend` unTag tag
    {-# INLINE begin #-}
    end :: Utf8Builder
    end = B.optimizePiece $ B.fromText "</" `mappend` unTag tag
                                            `mappend` B.fromChar '>'
    {-# INLINE end #-}
{-# INLINE parent #-}

-- | Create an HTML leaf element.
--
leaf :: Tag    -- ^ HTML element tag.
     -> Html a  -- ^ Resulting HTML.
leaf tag = Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` end
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromChar '<' `mappend` unTag tag
    {-# INLINE begin #-}
    end :: Utf8Builder
    end = B.optimizePiece $ B.fromText $ " />"
    {-# INLINE end #-}
{-# INLINE leaf #-}

-- | Produce an open tag. This can be used for open tags in HTML 4.01, like
-- for example @<br>@.
--
open :: Tag     -- ^ Tag for the open HTML element.
     -> Html a  -- ^ Resulting HTML.
open tag = Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.fromChar '>'
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromChar '<' `mappend` unTag tag
    {-# INLINE begin #-}
{-# INLINE open #-}

-- | Create an HTML attribute.
--
attribute :: Tag             -- ^ Key for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute key value = Attribute $ \(Html h) -> Html $ \attrs ->
    h $ attrs `mappend` begin
              `mappend` attributeValue value
              `mappend` B.fromChar '"'
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromChar ' ' `mappend` unTag key
                                             `mappend` B.fromText "=\""
{-# INLINE attribute #-}

class Attributable h where
    -- | Apply an attribute to an element.
    --
    -- Example:
    --
    -- > img ! src "foo.png"
    --
    -- Result:
    --
    -- > <img src="foo.png" />
    --
    -- This can be used on nested elements as well.
    --
    -- Example:
    --
    -- > p ! style "float: right" $ "Hello!"
    --
    -- Result:
    --
    -- > <p style="float: right">Hello!</p>
    --
    (!) :: h -> Attribute -> h

instance Attributable (Html a) where
    h ! (Attribute f) = f h
    {-# INLINE (!) #-}

instance Attributable (Html a -> Html b) where
    h ! (Attribute f) = f . h
    {-# INLINE (!) #-}

-- | Render text. This is the preferred way of converting string
-- datatypes to HTML.
--
text :: Text    -- ^ Text to render.
     -> Html a  -- ^ Resulting HTML fragment.
text = Html . const . B.escapeHtmlFromText
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Text    -- ^ Text to insert.
               -> Html a  -- Resulting HTML fragment.
preEscapedText = Html . const . B.fromText
{-# INLINE preEscapedText #-}

-- | Create an HTML snippet from a 'String'.
--
string :: String  -- ^ String to insert.
       -> Html a  -- ^ Resulting HTML fragment.
string = Html . const . B.escapeHtmlFromString
{-# INLINE string #-}

-- | Create an HTML snippet from a 'String' without escaping
--
preEscapedString :: String -> Html a
preEscapedString = Html . const . B.fromString
{-# INLINE preEscapedString #-}

-- | Create an HTML snippet from a datatype that instantiates 'Show'.
--
showHtml :: Show a
         => a       -- ^ Value to insert.
         -> Html b  -- ^ Resulting HTML fragment.
showHtml = string . show
{-# INLINE showHtml #-}

-- | Create an HTML snippet from a datatype that instantiates 'Show'. This
-- function will not do any HTML entity escaping.
--
preEscapedShowHtml :: Show a
                   => a       -- ^ Value to insert.
                   -> Html b  -- ^ Resulting HTML fragment.
preEscapedShowHtml = preEscapedString . show
{-# INLINE preEscapedShowHtml #-}

-- | Insert a 'ByteString'. This is an unsafe operation:
--
-- * The 'ByteString' could have the wrong encoding.
--
-- * The 'ByteString' might contain illegal HTML characters (no escaping is
--   done).
--
unsafeByteString :: ByteString  -- ^ Value to insert.
                 -> Html a      -- ^ Resulting HTML fragment.
unsafeByteString = Html . const . B.unsafeFromByteString
{-# INLINE unsafeByteString #-}

-- | Create a tag from a 'Text' value. A tag is a string used to denote a
-- certain HTML element, for example @img@.
--
-- This is only useful if you want to create custom HTML combinators.
--
textTag :: Text  -- ^ 'Text' for the tag.
        -> Tag   -- ^ Resulting tag.
textTag = Tag . B.fromText
{-# INLINE textTag #-}

-- | Create a tag from a 'String' value. For more information, see 'textTag'.
--
stringTag :: String  -- ^ 'String' for the tag.
          -> Tag     -- ^ Resulting tag.
stringTag = Tag . B.fromString
{-# INLINE stringTag #-}

-- | Render an attribute value from 'Text'.
--
textValue :: Text            -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . B.escapeHtmlFromText
{-# INLINE textValue #-}

-- | Render an attribute value from 'Text' without escaping.
--
preEscapedTextValue :: Text            -- ^ Text to insert.
                    -> AttributeValue  -- Resulting HTML fragment.
preEscapedTextValue = AttributeValue . B.fromText
{-# INLINE preEscapedTextValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . B.escapeHtmlFromString
{-# INLINE stringValue #-}

-- | Create an attribute value from a 'String' without escaping.
--
preEscapedStringValue :: String -> AttributeValue
preEscapedStringValue = AttributeValue . B.fromString
{-# INLINE preEscapedStringValue #-}

-- | /O(n)./ Render the HTML fragment to lazy 'L.ByteString'.
--
renderHtml :: Html a        -- ^ Document to render.
           -> L.ByteString  -- ^ Resulting output.
renderHtml = B.toLazyByteString . flip unHtml mempty
{-# INLINE renderHtml #-}
