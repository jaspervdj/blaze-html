{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification #-}
-- | The BlazeHtml core, consisting of functions that offer the power to
-- generate custom HTML elements. It also offers user-centric functions, which
-- are exposed through 'Text.Blaze'.
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
    ) where

import Control.Monad.Writer
import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Exts (IsString (..))

-- | A static string that supports efficient output to all possible backends.
--
data StaticString = StaticString
    { getString         :: String        -- ^ Regular Haskell string
    , getUtf8ByteString :: S.ByteString  -- ^ UTF-8 encoded bytestring
    , getText           :: Text          -- ^ Text value
    }

-- 'StaticString's should only be converted from string literals, as far as I
-- can see.
--
instance IsString StaticString where
    fromString s = let t = T.pack s
                   in StaticString s (T.encodeUtf8 t) t

-- | A string denoting input from different string representations.
--
data ChoiceString
    = Static     StaticString                       -- ^ Static data.
    | String     String                             -- ^ A Haskell String
    | Text       Text                               -- ^ A Text value
    | ByteString S.ByteString                       -- ^ An encoded bytestring
    | PreEscaped ChoiceString                       -- ^ A pre-escaped string
    | AppendChoiceString ChoiceString ChoiceString  -- ^ Concatenation.
    | EmptyChoiceString                             -- ^ Empty.

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
    -- | Open tag, end tag, content
    = forall b. Parent StaticString StaticString (HtmlM b)
    -- | Open tag, end tag
    | Leaf StaticString StaticString
    -- | Open tag, end tag
    | Open StaticString StaticString
    -- | HTML content
    | Content ChoiceString
    -- | Concatenation of two HTML pieces
    | forall b c. Append (HtmlM b) (HtmlM c)
    -- | Add an attribute to the inner HTML. Key, value, HTML to receive the
    -- attribute.
    | AddAttribute StaticString ChoiceString (HtmlM a)
    -- | Add a custom attribute to the inner HTML.
    | AddCustomAttribute ChoiceString ChoiceString (HtmlM a)
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
    deriving (IsString)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Shared key string for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute key value = Attribute $
    AddAttribute (unTag key) (unAttributeValue value)
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
dataAttribute tag value = Attribute $
    AddCustomAttribute
        (Static "data-" `mappend` Static (unTag tag) `mappend` "=\"")
        (unAttributeValue value)
{-# INLINE dataAttribute #-}

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

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: Text  -- ^ Text to render.
     -> Html  -- ^ Resulting HTML fragment.
text = Content . Text
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Text  -- ^ Text to insert.
               -> Html  -- Resulting HTML fragment.
preEscapedText = Content . PreEscaped . Text
{-# INLINE preEscapedText #-}

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
textTag t = Tag $ StaticString (T.unpack t) (T.encodeUtf8 t) t

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
preEscapedTextValue :: Text            -- ^ Text to insert.
                    -> AttributeValue  -- Resulting HTML fragment.
preEscapedTextValue = AttributeValue . PreEscaped . Text
{-# INLINE preEscapedTextValue #-}

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
