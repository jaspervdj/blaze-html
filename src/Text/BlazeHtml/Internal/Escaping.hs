{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.Internal.Escaping
    ( escapeHtml
    ) where

import Data.Text (Text)
import qualified Data.Text as T

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '&'  = "&amp;"
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '"'  = "&quot;"
    escape '\'' = "&#x27;"
    escape '/'  = "&#x2F;"
    escape x    = T.singleton x
