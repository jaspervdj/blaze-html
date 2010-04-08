{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal.HtmlMonad where

import Data.Monoid (Monoid, mconcat, mempty, mappend)

import GHC.Exts (IsString (..))

import Internal.Html
import Internal.Escaping

newtype ConcatenatedHtml h a = ConcatenatedHtml 
    { concatenatedHtml :: h
    } deriving (Monoid, UnicodeSequence, Encoded, Html)
    
instance Monoid h => Monad (ConcatenatedHtml h) where
    return a = ConcatenatedHtml mempty
    (ConcatenatedHtml h1) >> (ConcatenatedHtml h2) =
        ConcatenatedHtml $ h1 `mappend` h2
    (ConcatenatedHtml h1) >>= f = ConcatenatedHtml $ 
        let ConcatenatedHtml h2 = f undefined
        in h1 `mappend` h2

instance Html h => IsString (ConcatenatedHtml h a) where
    fromString = escapeHtmlContent . unicodeString

newtype SeparatedHtml h a = SeparatedHtml 
    { separatedHtml :: h
    } deriving (Monoid, UnicodeSequence, Encoded, Html)
    
instance Html h => Monad (SeparatedHtml h) where
    return a = SeparatedHtml mempty
    (SeparatedHtml h1) >> (SeparatedHtml h2) =
        SeparatedHtml $ h1 `separate` h2
    (SeparatedHtml h1) >>= f = SeparatedHtml $ 
        let SeparatedHtml h2 = f undefined
        in h1 `separate` h2

instance Html h => IsString (SeparatedHtml h a) where
    fromString = escapeHtmlContent . unicodeString
