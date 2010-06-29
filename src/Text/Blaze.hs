{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances #-}
-- | Core exposed functions.
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

      -- * Converting values to HTML.
    , text
    , preEscapedText
    , string
    , preEscapedString
    , showHtml
    , preEscapedShowHtml

      -- * Inserting literal ByteString's.
    , unsafeByteString

      -- * Creating tags.
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

import Text.Blaze.Internal
