{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | BlazeHtml is an HTML combinator library. It provides a way to embed HTML in
-- Haskell in an efficient and convenient way, with a light-weight syntax.
--
-- To use the library, one needs to import a set of HTML combinators. For
-- example, you can use HTML 4 Strict.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Prelude hiding (head, id, div)
-- > import Text.Blaze.Html4.Strict hiding (map)
-- > import Text.Blaze.Html4.Strict.Attributes hiding (title)
--
-- To render the page later on, you need a so called Renderer. The recommended
-- renderer is an UTF-8 renderer which produces a lazy bytestring.
--
-- > import Text.Blaze.Renderer.Utf8 (renderHtml)
--
-- Now, you can describe pages using the imported combinators.
--
-- > page1 :: Html
-- > page1 = html $ do
-- >     head $ do
-- >         title "Introduction page."
-- >         link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
-- >     body $ do
-- >         div ! id "header" $ "Syntax"
-- >         p "This is an example of BlazeHtml syntax."
-- >         ul $ mapM_ (li . toHtml . show) [1, 2, 3]
--
-- The resulting HTML can now be extracted using:
--
-- > renderHtml page1
--
module Text.Blaze
    (
      -- * Important types.
      Html
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating attributes.
    , dataAttribute
    , customAttribute

      -- * Converting values to HTML.
    , ToHtml (..)
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

      -- * Creating tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , ToValue (..)
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue
    , unsafeLazyByteStringValue

      -- * Setting attributes
    , (!)
    ) where

import Data.Monoid (mconcat)

import Data.Text (Text)
import qualified Data.Text.Lazy as LT

import Text.Blaze.Internal

-- | Class allowing us to use a single function for HTML values
--
class ToHtml a where
    -- | Convert a value to HTML.
    --
    toHtml :: a -> Html

instance ToHtml Html where
    toHtml = id
    {-# INLINE toHtml #-}

instance ToHtml [Html] where
    toHtml = mconcat
    {-# INLINE toHtml #-}

instance ToHtml Text where
    toHtml = text
    {-# INLINE toHtml #-}

instance ToHtml LT.Text where
    toHtml = lazyText
    {-# INLINE toHtml #-}

instance ToHtml String where
    toHtml = string
    {-# INLINE toHtml #-}

instance ToHtml Int where
    toHtml = string . show
    {-# INLINE toHtml #-}

instance ToHtml Char where
    toHtml = string . return
    {-# INLINE toHtml #-}

instance ToHtml Bool where
    toHtml = string . show
    {-# INLINE toHtml #-}

instance ToHtml Integer where
    toHtml = string . show
    {-# INLINE toHtml #-}

instance ToHtml Float where
    toHtml = string . show
    {-# INLINE toHtml #-}

instance ToHtml Double where
    toHtml = string . show
    {-# INLINE toHtml #-}

-- | Class allowing us to use a single function for attribute values
--
class ToValue a where
    -- | Convert a value to an HTML attribute value
    --
    toValue :: a -> AttributeValue

instance ToValue AttributeValue where
    toValue = id
    {-# INLINE toValue #-}

instance ToValue Text where
    toValue = textValue
    {-# INLINE toValue #-}

instance ToValue LT.Text where
    toValue = lazyTextValue
    {-# INLINE toValue #-}

instance ToValue String where
    toValue = stringValue
    {-# INLINE toValue #-}

instance ToValue Int where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Char where
    toValue = stringValue . return
    {-# INLINE toValue #-}

instance ToValue Bool where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Integer where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Float where
    toValue = stringValue . show
    {-# INLINE toValue #-}

instance ToValue Double where
    toValue = stringValue . show
    {-# INLINE toValue #-}
