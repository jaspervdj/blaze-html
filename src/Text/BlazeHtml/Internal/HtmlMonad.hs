module Text.BlazeHtml.Internal.HtmlMonad
    ( HtmlMonad (..)
    ) where

import Data.Monoid

import Text.BlazeHtml.Internal.Html

newtype HtmlMonad h a = HtmlMonad { runHtmlMonad :: h }

instance (Monoid h) => Monoid (HtmlMonad h a) where
    mempty                                = HtmlMonad mempty
    mappend (HtmlMonad h1) (HtmlMonad h2) = HtmlMonad $ h1 `mappend` h2

instance (Html h) => Html (HtmlMonad h a) where
    renderUnescapedText t = HtmlMonad $ renderUnescapedText t
    renderLeafElement t = HtmlMonad $ renderLeafElement t
    renderElement t (HtmlMonad h) = HtmlMonad $ renderElement t h
    modifyUnescapedAttributes f (HtmlMonad h) =
        HtmlMonad $ modifyUnescapedAttributes f h
    
instance (Monoid h) => Monad (HtmlMonad h) where
    return   = mempty
    (HtmlMonad h1) >> (HtmlMonad h2) = HtmlMonad $ h1 `mappend` h2
    (HtmlMonad h1) >>= f = let HtmlMonad h2 = f errorMessage
                           in HtmlMonad $ h1 `mappend` h2
      where
        errorMessage = error "HtmlMonad: >>= returning values not supported."
