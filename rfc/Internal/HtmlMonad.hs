{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal.HtmlMonad 
    ( ConcatenatedHtml (..)
    , SeparatedHtml (..)
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import GHC.Exts (IsString)

import Internal.Html
