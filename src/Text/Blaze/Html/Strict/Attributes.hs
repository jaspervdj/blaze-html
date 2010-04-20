{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html.Strict.Attributes
    ( id
    ) where

import Prelude hiding (id)

import Text.Blaze.Internal.Html

id = addAttribute "id"
