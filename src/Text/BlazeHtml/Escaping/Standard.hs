{-# LANGUAGE OverloadedStrings #-}
-- | Standard escaping module. Nothing fancy, but it will prevent XSS attacks.
module Text.BlazeHtml.Escaping.Standard
    ( escapeHtml
    ) where


import Control.Arrow (second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid

import Text.BlazeHtml.Text as T hiding (map)

-- | Escape HTML. This does not change the encoding, it only does escaping to
-- prevent XSS attacks. If you want to do encoding (translate non-standard
-- characters into HTML entities, you need to look at the
-- @Text.BlazeHtml.Encoding@ module instead.
escapeHtml :: Monoid m => (Text -> m) -> Text -> m
escapeHtml toMonoid = T.foldr folder mempty
  where
    folder c chunk = fromMaybe (toMonoid $ T.singleton c) (M.lookup c characters)
                   `mappend` chunk
    characters = M.fromList $ map (second toMonoid) $ [ ('&', "&amp;")
                                                      , ('<', "&lt;")
                                                      , ('>', "&gt;")
                                                      , ('"', "&quot;")
                                                      , ('\'', "&#x27;")
                                                      , ('/', "&#x2F;")
                                                      ]
