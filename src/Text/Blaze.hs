{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
-- | Core exposed functions.
module Text.Blaze
    ( 
      -- * Important types.
      Html
    , Attribute

      -- * Creating custom tags and attributes.
    , parent
    , leaf
    , attribute
    , open

      -- * Converting values to HTML.
    , text
    , escapedText
    , string
    , escapedString

      -- * Setting attributes
    , (!)

      -- * Rendering HTML.
    , renderHtml
    ) where

import Data.Monoid (Monoid, mappend, mempty, mconcat)

import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import GHC.Exts (IsString (..))

import qualified Text.Blaze.Internal.Utf8Builder as B
--SM: I didn't notice in the Utf8Builder file, but I do know here. I would
--strongly suggest to use a newtype to differentiate between Utf8Builder and a
--standard Builder. A Utf8 builder has different invarians and supports
--operations not supportable by a builder (e.g. toText).

-- | The core HTML datatype.
--
newtype HtmlM a = HtmlM
    { -- | Function to extract the 'Builder'.
      --
      -- SM: I would expect this function to be:
      --
      --  Utf8Builder -> Utf8Builder
      --
      runHtml :: Builder -> Builder
    }

-- | Simplification of the 'HtmlM' type.
--
type Html = HtmlM ()

newtype Attribute = Attribute (Html -> Html)

instance Monoid (HtmlM a) where
    mempty = HtmlM $ \_ -> mempty
    {-# INLINE mempty #-}
    --SM: Note for the benchmarks: We should test which multi-`mappend`
    --versions are faster: right or left-associative ones. Then we can register
    --a rewrite rule taking care of that. I actually guess that this may be one
    --of the reasons accounting for the speed differences between monadic
    --syntax and monoid syntax: the rewrite rules for monadic syntax bring the
    --`>>=` into the better form which results in a better form for `mappend`.
    (HtmlM h1) `mappend` (HtmlM h2) = HtmlM $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = HtmlM $ \attrs ->
        foldr (\h k -> runHtml h attrs `mappend` k) mempty hs
    {-# INLINE mconcat #-}

instance Monad HtmlM where
    return _ = mempty
    {-# INLINE return #-}
    (HtmlM h1) >> (HtmlM h2) = HtmlM $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}

instance IsString Html where
    fromString = string
    {-# INLINE fromString #-}


-- | Create an HTML parent element.
--
parent :: S.ByteString -> Html -> Html
parent tag = \inner -> HtmlM $ \attrs ->
    B.fromEscapedByteString begin
      `mappend` attrs
      `mappend` B.fromEscapedAscii7Char '>'
      `mappend` runHtml inner mempty
      `mappend` B.fromEscapedByteString end
  where
    begin :: ByteString
    begin = "<" `mappend` tag
    end :: ByteString
    end = "</" `mappend` tag `mappend` ">"
{-# INLINE parent #-}

-- | Create an HTML leaf element.
--
leaf :: S.ByteString -> Html
leaf tag = HtmlM $ \attrs ->
    B.fromEscapedByteString begin
      `mappend` attrs
      `mappend` B.fromEscapedByteString " />"
  where
    begin :: ByteString
    begin = "<" `mappend` tag
{-# INLINE leaf #-}

-- | Produce an open tag. This can be used for open tags in HTML 4.01, like
-- for example @<br>@.
--
open :: S.ByteString -> Html
open tag = HtmlM $ \attrs ->
    B.fromEscapedByteString begin
      `mappend` attrs
      `mappend` B.fromEscapedByteString ">"
  where
    begin :: ByteString
    begin = "<" `mappend` tag
{-# INLINE open #-}

-- | Add an attribute to the current element.
--
-- SM: Why are attribute values fixed to 'Text'. Couldn't it be that the
-- attribute comes from an 'Int' and we want to use 'show' to build it. Couldn't
-- it also be that the attribute needs to be composed also?
--
-- I would suggest using a newtype 'AttributeValue' that encapsulates a
-- Utf8Builder. Then we can provide the correct escaping (doublequoted attributes
-- need less escaping than html content) and combinators for building attribute
-- values; i.e. an IsString instance, overload 'string', 'text' and 'fromShow'.
--
attribute :: S.ByteString -> Text -> Attribute
attribute key value = Attribute $ \(HtmlM h) -> HtmlM $ \attrs ->
    h $ attrs `mappend` B.fromEscapedByteString begin
              `mappend` B.fromText value
              `mappend` B.fromEscapedAscii7Char '"'
  where
    begin :: ByteString
    begin = " " `mappend` key `mappend` "=\""
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

-- | Render escaped text. This is the preferred way of converting string
-- datatypes to HTML.
--
text :: Text  -- ^ Text to render.
     -> Html  -- ^ Resulting HTML fragment.
text = HtmlM . const . B.fromText
{-# INLINE text #-}

-- | Render text without escaping.
--
escapedText :: Text  -- ^ Text to insert.
            -> Html  -- Resulting HTML fragment.
escapedText = HtmlM . const . B.fromEscapedText
{-# INLINE escapedText #-}

-- | Create a HTML snippet from a 'String'.
--
string :: String -> Html
string = HtmlM . const . B.fromString
{-# INLINE string #-}

-- Why not provide a 'fromShow :: Show a => a -> Html' method?

-- | Create a HTML snippet from a 'String' without escaping
--
-- SM: Somehow I'm not sure if 'escaped' is the right word here. I read it now
-- quite a few times in the source files. From the word itself I cannot tell,
-- if it means that escaping is being done or not. Why not switch to
-- 'preescaped' ?
--
escapedString :: String -> Html
escapedString = HtmlM . const . B.fromEscapedString
{-# INLINE escapedString #-}

-- | /O(n)./ Render the HTML fragment to lazy 'L.ByteString'.
--
renderHtml :: Html -> L.ByteString
renderHtml = toLazyByteString . flip runHtml mempty
