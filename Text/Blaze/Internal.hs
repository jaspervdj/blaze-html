{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification #-}
-- | The BlazeHtml core, consisting of functions that offer the power to
-- generate custom HTML elements. It also offers user-centric functions, which
-- are exposed through 'Text.Blaze'.
--
-- While this module is exported, usage of it is not recommended, unless you
-- know what you are doing. This module might undergo changes at any time.
--
module Text.Blaze.Internal
    (
      -- * Important types.
      ChoiceString (..)
    , StaticString (..)
    , HtmlM (..)
    , Html
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating custom tags and attributes.
    , attribute
    , dataAttribute
    , customAttribute

      -- * Converting values to HTML.
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , string
    , preEscapedString
    , showHtml
    , preEscapedShowHtml
    , unsafeByteString

      -- * Converting values to tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue

      -- * Setting attributes
    , (!)

      -- * Modifying HTML elements
    , external
    ) where

import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import GHC.Exts (IsString (..))

-- | A static string that supports efficient output to all possible backends.
--
data StaticString = StaticString
    { getString         :: String -> String  -- ^ Appending haskell string
    , getUtf8ByteString :: S.ByteString      -- ^ UTF-8 encoded bytestring
    , getText           :: Text              -- ^ Text value
    }

-- 'StaticString's should only be converted from string literals, as far as I
-- can see.
--
instance IsString StaticString where
    fromString s = let t = T.pack s
                   in StaticString (s ++) (T.encodeUtf8 t) t

-- | A string denoting input from different string representations.
--
data ChoiceString
    -- | Static data
    = Static {-# UNPACK #-} !StaticString
    -- | A Haskell String
    | String String
    -- | A Text value
    | Text Text
    -- | An encoded bytestring
    | ByteString S.ByteString
    -- | A pre-escaped string
    | PreEscaped ChoiceString
    -- | External data in style/script tags, should be checked for validity
    | External ChoiceString
    -- | Concatenation
    | AppendChoiceString ChoiceString ChoiceString
    -- | Empty string
    | EmptyChoiceString

instance Monoid ChoiceString where
    mempty = EmptyChoiceString
    {-# INLINE mempty #-}
    mappend = AppendChoiceString
    {-# INLINE mappend #-}

instance IsString ChoiceString where
    fromString = String
    {-# INLINE fromString #-}

-- | The core HTML datatype.
--
data HtmlM a
    -- | Tag, open tag, end tag, content
    = forall b. Parent StaticString StaticString StaticString (HtmlM b)
    -- | Tag, open tag, end tag
    | Leaf StaticString StaticString StaticString
    -- | HTML content
    | Content ChoiceString
    -- | Concatenation of two HTML pieces
    | forall b c. Append (HtmlM b) (HtmlM c)
    -- | Add an attribute to the inner HTML. Raw key, key, value, HTML to
    -- receive the attribute.
    | AddAttribute StaticString StaticString ChoiceString (HtmlM a)
    -- | Add a custom attribute to the inner HTML.
    | AddCustomAttribute ChoiceString ChoiceString ChoiceString (HtmlM a)
    -- | Empty HTML.
    | Empty

-- | Simplification of the 'HtmlM' datatype.
--
type Html = HtmlM ()

instance Monoid a => Monoid (HtmlM a) where
    mempty = Empty
    {-# INLINE mempty #-}
    mappend x y = Append x y
    {-# INLINE mappend #-}
    mconcat = foldr Append Empty
    {-# INLINE mconcat #-}

instance Monad HtmlM where
    return _ = Empty
    {-# INLINE return #-}
    (>>) = Append
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}

instance IsString (HtmlM a) where
    fromString = Content . fromString
    {-# INLINE fromString #-}

-- | Type for an HTML tag. This can be seen as an internal string type used by
-- BlazeHtml.
--
newtype Tag = Tag { unTag :: StaticString }
    deriving (IsString)

-- | Type for an attribute.
--
newtype Attribute = Attribute (forall a. HtmlM a -> HtmlM a)

-- | The type for the value part of an attribute.
--
newtype AttributeValue = AttributeValue { unAttributeValue :: ChoiceString }
    deriving (IsString, Monoid)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Raw key
          -> Tag             -- ^ Shared key string for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute rawKey key value = Attribute $
    AddAttribute (unTag rawKey) (unTag key) (unAttributeValue value)
{-# INLINE attribute #-}

-- | From HTML 5 onwards, the user is able to specify custom data attributes.
--
-- An example:
--
-- > <p data-foo="bar">Hello.</p>
--
-- We support this in BlazeHtml using this funcion. The above fragment could
-- be described using BlazeHtml with:
--
-- > p ! dataAttribute "foo" "bar" $ "Hello."
--
dataAttribute :: Tag             -- ^ Name of the attribute.
              -> AttributeValue  -- ^ Value for the attribute.
              -> Attribute       -- ^ Resulting HTML attribute.
dataAttribute tag value = Attribute $ AddCustomAttribute
    (Static "data-" `mappend` Static (unTag tag))
    (Static " data-" `mappend` Static (unTag tag) `mappend` Static "=\"")
    (unAttributeValue value)
{-# INLINE dataAttribute #-}

-- | Create a custom attribute. This is not specified in the HTML spec, but some
-- JavaScript libraries rely on it.
--
-- An example:
--
-- > <select dojoType="select">foo</select>
--
-- Can be produced using:
--
-- > select ! customAttribute "dojoType" "select" $ "foo"
--
customAttribute :: Tag             -- ^ Name of the attribute
                -> AttributeValue  -- ^ Value for the attribute
                -> Attribute       -- ^ Resulting HTML attribtue
customAttribute tag value = Attribute $ AddCustomAttribute
    (Static $ unTag tag)
    (Static " " `mappend` Static (unTag tag) `mappend` Static "=\"")
    (unAttributeValue value)
{-# INLINE customAttribute #-}

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: Text  -- ^ Text to render.
     -> Html  -- ^ Resulting HTML fragment.
text = Content . Text
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Text  -- ^ Text to insert
               -> Html  -- ^ Resulting HTML fragment
preEscapedText = Content . PreEscaped . Text
{-# INLINE preEscapedText #-}

-- | A variant of 'text' for lazy 'LT.Text'.
--
lazyText :: LT.Text  -- ^ Text to insert
         -> Html     -- ^ Resulting HTML fragment
lazyText = mconcat . map text . LT.toChunks
{-# INLINE lazyText #-}

-- | A variant of 'preEscapedText' for lazy 'LT.Text'
--
preEscapedLazyText :: LT.Text  -- ^ Text to insert
                   -> Html     -- ^ Resulting HTML fragment
preEscapedLazyText = mconcat . map preEscapedText . LT.toChunks

-- | Create an HTML snippet from a 'String'.
--
string :: String  -- ^ String to insert.
       -> Html    -- ^ Resulting HTML fragment.
string = Content . String
{-# INLINE string #-}

-- | Create an HTML snippet from a 'String' without escaping
--
preEscapedString :: String  -- ^ String to insert.
                 -> Html    -- ^ Resulting HTML fragment.
preEscapedString = Content . PreEscaped . String
{-# INLINE preEscapedString #-}

-- | Create an HTML snippet from a datatype that instantiates 'Show'.
--
showHtml :: Show a
         => a       -- ^ Value to insert.
         -> Html    -- ^ Resulting HTML fragment.
showHtml = string . show
{-# INLINE showHtml #-}

-- | Create an HTML snippet from a datatype that instantiates 'Show'. This
-- function will not do any HTML entity escaping.
--
preEscapedShowHtml :: Show a
                   => a     -- ^ Value to insert.
                   -> Html  -- ^ Resulting HTML fragment.
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
                 -> Html        -- ^ Resulting HTML fragment.
unsafeByteString = Content . ByteString
{-# INLINE unsafeByteString #-}

-- | Create a 'Tag' from some 'Text'.
--
textTag :: Text  -- ^ Text to create a tag from
        -> Tag   -- ^ Resulting tag
textTag t = Tag $ StaticString (T.unpack t ++) (T.encodeUtf8 t) t

-- | Create a 'Tag' from a 'String'.
--
stringTag :: String  -- ^ String to create a tag from
          -> Tag     -- ^ Resulting tag
stringTag = Tag . fromString

-- | Render an attribute value from 'Text'.
--
textValue :: Text            -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . Text
{-# INLINE textValue #-}

-- | Render an attribute value from 'Text' without escaping.
--
preEscapedTextValue :: Text            -- ^ The actual value
                    -> AttributeValue  -- ^ Resulting attribute value
preEscapedTextValue = AttributeValue . PreEscaped . Text
{-# INLINE preEscapedTextValue #-}

-- | A variant of 'textValue' for lazy 'LT.Text'
--
lazyTextValue :: LT.Text         -- ^ The actual value
              -> AttributeValue  -- ^ Resulting attribute value
lazyTextValue = mconcat . map textValue . LT.toChunks
{-# INLINE lazyTextValue #-}

-- | A variant of 'preEscapedTextValue' for lazy 'LT.Text'
--
preEscapedLazyTextValue :: LT.Text         -- ^ The actual value
                        -> AttributeValue  -- ^ Resulting attribute value
preEscapedLazyTextValue = mconcat . map preEscapedTextValue . LT.toChunks
{-# INLINE preEscapedLazyTextValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . String
{-# INLINE stringValue #-}

-- | Create an attribute value from a 'String' without escaping.
--
preEscapedStringValue :: String -> AttributeValue
preEscapedStringValue = AttributeValue . PreEscaped . String
{-# INLINE preEscapedStringValue #-}

-- | Create an attribute value from a 'ByteString'. See 'unsafeByteString'
-- for reasons why this might not be a good idea.
--
unsafeByteStringValue :: ByteString      -- ^ ByteString value
                      -> AttributeValue  -- ^ Resulting attribute value
unsafeByteStringValue = AttributeValue . ByteString

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

instance Attributable (HtmlM a) where
    h ! (Attribute f) = f h
    {-# INLINE (!) #-}

instance Attributable (HtmlM a -> HtmlM b) where
    h ! f = (! f) . h
    {-# INLINE (!) #-}

-- | Mark HTML as external data. External data can be:
--
-- * CSS data in a @<style>@ tag;
--
-- * Script data in a @<script>@ tag.
--
-- This function is applied automatically when using the @style@ or @script@
-- combinators.
--
external :: HtmlM a -> HtmlM a
external (Content x) = Content $ External x
external (Append x y) = Append (external x) (external y)
external (Parent x y z i) = Parent x y z $ external i
external (AddAttribute x y z i) = AddAttribute x y z $ external i
external (AddCustomAttribute x y z i) = AddCustomAttribute x y z $ external i
external x = x
{-# INLINE external #-}
