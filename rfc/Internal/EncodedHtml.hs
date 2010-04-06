{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | Encoding and rendering independent first-class values for representing
-- Html and other tree-like documents.
module Internal.EncodedHtml where

import Prelude hiding (head)
import Data.Monoid (mempty)

import Data.Text (Text)

import Internal.UnicodeSequence

-- | A typeclass represeting a certain encoding.
class UnicodeSequence h => Encoded h where
    -- | The tag marking the encoding
    encodingTag        :: h
    -- | Replace all unencodable characters using the given character
    -- substitution.
    replaceUnencodable :: (Char -> h) -> h -> h

-- | The main Html typeclass.
class Encoded h => Html h where
    -- | Left html, right html
    separate     :: h -> h -> h
    -- | Tag
    leafElement  :: h -> h
    -- | Tag, inner html
    nodeElement  :: h -> h -> h
    -- | Key, value, html taking attributes
    addAttribute :: h -> h -> h -> h

-- | Put a character without doing escaping.
unescapedChar :: UnicodeSequence s => Char -> s
unescapedChar = unicodeChar

-- | Put some text without doing escaping.
unescapedText :: UnicodeSequence s => Text -> s
unescapedText = unicodeText

instance Encoded h => Encoded (h -> h) where
    encodingTag = const encodingTag
    replaceUnencodable subst h = 
        const $ replaceUnencodable (\c -> subst c mempty) (h mempty)

-- We require this instance for allowing to addAttributes to nesting
-- html combinators; i.e. combinators of type 'h -> h'. Except for
-- the third argument of 'addAttribute' the outer argument is not
-- passed through. This is to avoid unnecessary references to these
-- arguments.
--
-- TODO: Check what inlining (and if required specialization) is necessary to
-- get rid of the abstraction cost.
instance Html h => Html (h -> h) where
    h1 `separate` h2    = const $ h1 mempty `separate` h2 mempty
    leafElement h       = const $ h mempty
    nodeElement h inner = const $ nodeElement (h mempty) (inner mempty)
    addAttribute key value h = \inner ->
        addAttribute (key mempty) (value mempty) (h inner)
