{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | Encoding and rendering independent first-class values for representing
-- Html and other tree-like documents.
module Internal.Html where

import Prelude hiding (head)
import Data.Monoid (Monoid, mconcat, mempty, mappend)

import Data.Text (Text)

-- | A @UnicodeSequence@ is a type that can represent sequences built from
-- unicode characters represented as standard Haskell Char's and subsequences
-- of unicode characters represented as @Text@ values from @Data.Text@.
--
-- NOTE that there is no instance @UnicodeSequence s => UnicodeSequence [s]@
-- because then the possibly slow list mappend (++) would be used instead of
-- the (fast) mappend of the underlying implementation of the unicode sequence.
class Monoid s => UnicodeSequence s where
    -- | A unicode character in the standard Haskell encoding represented by
    -- @Char@.
    unicodeChar :: Char -> s
    -- | A sequence of unicode characters represented as @Text@ from
    -- @Data.Text@.
    unicodeText :: Text -> s
    -- | A character that is guaranteed to be in one of the lowest 127
    -- characters. This method has a default implementation, but can be
    -- overwritten for performance reasons.
    ascii7Char :: Char -> s
    ascii7Char = unicodeChar

-- This instance is needed so we can automatically derive unfolded readers.
instance UnicodeSequence s => UnicodeSequence (a -> s) where
    unicodeChar   = const . unicodeChar
    unicodeText   = const . unicodeText

-- | Build a unicode sequence from a string.
unicodeString :: UnicodeSequence s => String -> s
unicodeString = mconcat . map unicodeChar

-- | Build a unicode sequence from a value that can be shown.
unicodeShow :: (UnicodeSequence s, Show a) => a -> s
unicodeShow = unicodeString . show

-- | Build a unicode sequence from a string only containing characters from
-- the @[0 .. 127]@ range.
ascii7String :: UnicodeSequence s => String -> s
ascii7String = mconcat . map ascii7Char

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
        const $ replaceUnencodable (flip subst mempty) (h mempty)

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
    addAttribute key value h inner =
        addAttribute (key mempty) (value mempty) (h inner)
