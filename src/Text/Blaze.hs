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
    , (<!)

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
      runHtml :: Utf8Builder -> Utf8Builder
    }

-- | Type for an attribute.
--
newtype Attribute a = Attribute (Html a -> Html a)

-- | The type for an attribute value.
--
newtype AttributeValue = AttributeValue { attributeValue :: Utf8Builder }

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
        foldr mappend mempty $ map (flip runHtml attrs) hs
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

instance IsString AttributeValue where
    fromString = stringValue
    {-# INLINE fromString #-}

-- | Create an HTML parent element.
--
parent :: Text    -- ^ HTML element tag.
       -> Html a  -- ^ Inner HTML, to place in this element.
       -> Html a  -- ^ Resulting HTML.
parent tag = \inner -> Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.fromChar '>'
      `mappend` runHtml inner mempty
      `mappend` end
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromText $ "<" `mappend` tag
    end :: Utf8Builder
    end = B.optimizePiece $ B.fromText $ "</" `mappend` tag
                                              `mappend` ">"
{-# INLINE parent #-}

-- | Create an HTML leaf element.
--
leaf :: Text    -- ^ HTML element tag.
     -> Html a  -- ^ Resulting HTML.
leaf tag = Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` end
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromText $ "<" `mappend` tag
    end :: Utf8Builder
    end = B.optimizePiece $ B.fromText $ " />"
{-# INLINE leaf #-}

-- | Produce an open tag. This can be used for open tags in HTML 4.01, like
-- for example @<br>@.
--
open :: Text    -- ^ Tag for the open HTML element.
     -> Html a  -- ^ Resulting HTML.
open tag = Html $ \attrs ->
    begin
      `mappend` attrs
      `mappend` B.fromChar '>'
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromText $ "<" `mappend` tag
{-# INLINE open #-}

-- | Create an HTML attribute.
--
attribute :: Text            -- ^ Key for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute a     -- ^ Resulting HTML attribute.
attribute key value = Attribute $ \(Html h) -> Html $ \attrs ->
    h $ attrs `mappend` begin
              `mappend` attributeValue value
              `mappend` B.fromChar '"'
  where
    begin :: Utf8Builder
    begin = B.optimizePiece $ B.fromText $ " " `mappend` key
                                               `mappend` "=\""
{-# INLINE attribute #-}

-- | Apply an attribute to a leaf element.
--
-- Example:
--
-- > img ! src "foo.png"
--
-- Result:
--
-- > <img src="foo.png" />
--
(!) :: Html a -> Attribute a -> Html a
h ! (Attribute f) = f h
{-# INLINE (!) #-}


-- | Apply an attribute to a parent element.
--
-- Example:
--
-- > p ! style "float: right" $ "Hello!"
--
-- Result:
--
-- > <p style="float: right">Hello!</p>
--
(<!) :: (Html a -> Html a)
     -> Attribute a
     -> (Html a -> Html a)
f <! (Attribute g) = \h -> g (f h)
{-# INLINE (<!) #-}

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

-- | Create a HTML snippet from a 'String'.
--
string :: String -> Html a
string = Html . const . B.escapeHtmlFromString
{-# INLINE string #-}

-- Why not provide a 'fromShow :: Show a => a -> Html' method?

-- | Create a HTML snippet from a 'String' without escaping
--
preEscapedString :: String -> Html a
preEscapedString = Html . const . B.fromString
{-# INLINE preEscapedString #-}

-- | Render an attrbitute value from 'Text'.
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
renderHtml :: Html a -> L.ByteString
renderHtml = B.toLazyByteString . flip runHtml mempty
{-# INLINE renderHtml #-}
