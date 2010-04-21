{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html.Strict.Attributes
    ( id
    ) where

import Prelude hiding (id)

import Data.Text (Text)

import Text.Blaze.Internal.Html

id :: Text -> Attribute
id = attribute "id"
