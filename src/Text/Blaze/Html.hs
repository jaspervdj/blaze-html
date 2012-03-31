{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}
module Text.Blaze.Html where

import Text.Blaze

type Html = Markup

toHtml :: forall a. ToMarkup a => a -> Markup 
toHtml = toMarkup