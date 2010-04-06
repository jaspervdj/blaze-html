{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An abstract representation of a sequence of Unicode characters with a
-- special focus on allowing for efficient transcoding.
module Internal.UnicodeSequence where

import Data.Monoid (Monoid, mconcat)

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
    unicodeChar    :: Char -> s
    -- | A sequence of unicode characters represented as @Text@ from
    -- @Data.Text@.
    unicodeText    :: Text -> s

-- This instance is needed so we can automatically derive unfolded readers.
instance UnicodeSequence s => UnicodeSequence (a -> s) where
    unicodeChar   = const . unicodeChar
    unicodeText   = const . unicodeText

-- | Build a unicode sequence from a string.
unicodeString :: UnicodeSequence s => String -> s
unicodeString = mconcat . map unicodeChar

-- | Build a unicode sequence from a value that can be shown.
unicodeShow :: UnicodeSequence s => String -> s
unicodeShow = unicodeString . show
