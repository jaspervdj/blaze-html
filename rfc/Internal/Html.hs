{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | Encoding and rendering independent first-class values for representing
-- Html and other tree-like documents.
module Internal.Html where

import Prelude hiding (head)
import Data.Monoid (Monoid, mconcat, mempty, mappend)

import GHC.Exts (IsString (..))
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

newtype ConcatenatedHtml h a = ConcatenatedHtml 
    { concatenatedHtml :: h
    } deriving (Monoid, UnicodeSequence, Encoded, Html, IsString)
    
instance Monoid h => Monad (ConcatenatedHtml h) where
    return a = ConcatenatedHtml mempty
    (ConcatenatedHtml h1) >> (ConcatenatedHtml h2) =
        ConcatenatedHtml $ h1 `mappend` h2
    (ConcatenatedHtml h1) >>= f = ConcatenatedHtml $ 
        let ConcatenatedHtml h2 = f undefined
        in h1 `mappend` h2

newtype SeparatedHtml h a = SeparatedHtml 
    { separatedHtml :: h
    } deriving (Monoid, UnicodeSequence, Encoded, Html, IsString)
    
instance Monoid h => Monad (SeparatedHtml h) where
    return a = SeparatedHtml mempty
    (SeparatedHtml h1) >> (SeparatedHtml h2) =
        SeparatedHtml $ h1 `mappend` h2
    (SeparatedHtml h1) >>= f = SeparatedHtml $ 
        let SeparatedHtml h2 = f undefined
        in h1 `mappend` h2
