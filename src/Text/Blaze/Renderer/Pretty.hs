-- | A renderer that produces pretty HTML, mostly meant for debugging purposes.
--
module Text.Blaze.Renderer.Pretty
    ( renderHtml
    ) where

import Text.Blaze.Internal
import Text.Blaze.Renderer.String (escapeHtmlEntities, fromChoiceString)

-- | Render some 'Html' to an appending 'String'.
--
renderString :: HtmlM a  -- ^ HTML to render
             -> String   -- ^ String to append
             -> String   -- ^ Resulting String
renderString = go 0 id
  where
    go :: Int -> (String -> String) -> HtmlM b -> String -> String
    go i attrs (Parent open close content) =
        ind i . getString open . attrs . (">\n" ++) . go (inc i) id content
              . ind i . getString close .  ('\n' :)
    go i attrs (Leaf begin end) =
        ind i . getString begin . attrs . getString end . ('\n' :)
    go i attrs (AddAttribute key value h) = flip (go i) h $
        getString key . fromChoiceString value . ('"' :) . attrs
    go i attrs (AddCustomAttribute key value h) = flip (go i) h $
        fromChoiceString key . fromChoiceString value . ('"' :) . attrs
    go i _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go i attrs (Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ Empty = id
    {-# NOINLINE go #-}

    -- Increase the indentation
    inc = (+) 4

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)
{-# INLINE renderString #-}

-- | Render HTML to a lazy 'String'. The result is prettified.
--
renderHtml :: HtmlM a  -- ^ HTML to render
           -> String   -- ^ Resulting 'String'.
renderHtml html = renderString html ""
{-# INLINE renderHtml #-}
