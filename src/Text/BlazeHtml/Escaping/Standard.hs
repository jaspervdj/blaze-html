{-# LANGUAGE OverloadedStrings #-}
-- | Standard escaping module. Nothing fancy, but it will prevent XSS attacks.
module Text.BlazeHtml.Escaping.Standard
    ( escapeHtml
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Text as T

-- | Escape HTML. This does not change the encoding, it only does escaping to
-- prevent XSS attacks. If you want to do encoding (translate non-standard
-- characters into HTML entities, you need to look at the
-- @Text.BlazeHtml.Encoding@ module instead.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeCharacter
  where
    escapeCharacter c = fromMaybe (T.singleton c) $ M.lookup c characters
    characters = M.fromList [ ('&', "&amp;")
                            , ('<', "&lt;")
                            , ('>', "&gt;")
                            , ('"', "&quot;")
                            , ('\'', "&#x27;")
                            , ('/', "&#x2F;")
                            ]
