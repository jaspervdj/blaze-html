-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.String
    ( renderHtml
    ) where

import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.Text as T
import qualified Data.ByteString as S

import Text.Blaze.Internal

-- | Escape HTML entities in a string
--
escapeHtmlEntities :: String  -- ^ String to escape
                   -> String  -- ^ String to append
                   -> String  -- ^ Resulting string
escapeHtmlEntities []     k = k
escapeHtmlEntities (c:cs) k = case c of
    '<'  -> "&lt;"   ++ escapeHtmlEntities cs k
    '>'  -> "&gt;"   ++ escapeHtmlEntities cs k
    '&'  -> "&amp;"  ++ escapeHtmlEntities cs k
    '"'  -> "&quot;" ++ escapeHtmlEntities cs k
    '\'' -> "&apos;" ++ escapeHtmlEntities cs k
    x    -> x : escapeHtmlEntities cs k

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = escapeHtmlEntities s
fromChoiceString (Text s)       = escapeHtmlEntities $ T.unpack s
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) = case x of
    String s -> (s ++)
    Text   s -> (T.unpack s ++)
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (T.unpack s ++)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
{-# INLINE fromChoiceString #-}

-- | Render some 'Html' to an appending 'String'.
--
renderString :: HtmlM a  -- ^ HTML to render
             -> String   -- ^ String to append
             -> String   -- ^ Resulting String
renderString = go id 
  where
    go :: (String -> String) -> HtmlM b -> String -> String
    go attrs (Parent open close content) k =
        getString open $ attrs $ '>' : go id content (getString close k)
    go attrs (Leaf begin end) k = 
        getString begin $ attrs $ getString end k
    go attrs (AddAttribute key value h) k =
        go (\k' -> getString key $ fromChoiceString value $ '"' : attrs k') h k
    go attrs (AddCustomAttribute key value h) k =
        go (\k' -> fromChoiceString key $
            fromChoiceString value $ '"' : attrs k') h k
    go _ (Content content) k = fromChoiceString content k
    go attrs (Append h1 h2) k = go attrs h1 $ go attrs h2 k
    go _ Empty k            = k
    {-# NOINLINE go #-}
{-# INLINE renderString #-}

-- | Render HTML to a lazy UTF-8 encoded 'L.ByteString.'
--
renderHtml :: HtmlM a  -- ^ HTML to render
           -> String   -- ^ Resulting 'String'
renderHtml html = renderString html ""
{-# INLINE renderHtml #-}
