{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal.HtmlMonad 
    ( ConcatenatedHtml (..)
    , SeparatedHtml (..)
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import GHC.Exts (IsString)

import Internal.EncodedHtml
import Internal.UnicodeSequence

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
