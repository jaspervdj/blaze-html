{-# LANGUAGE OverloadedStrings #-}
-- | A generic indenter for Html documents.
--
-- NOTE: This is only a prototype. It does not yet maintain the precise
-- semantics of white-space in an Html document. More care needs to be taken to
-- achieve the desired effect.
module Text.BlazeHtml.Render.Indent (
    Indented
  , indented
) where

import Prelude hiding (replicate)
import Data.Monoid

import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (replicate)

-- | Produce a Html text consisting of 'n' spaces.
spaces :: Html h => Int -> h
spaces n = unescapedText $ replicate n " "


-- | FIXME: Define clean indenting semantics and implement.
--
-- Currently, a difference is made between inner text block and inner html
-- blocks. However, this is not sufficient (cf. WoW content example).
--
-- NOTE: That preformated tags can be dealt with by directly encoding them as
-- unescaped text and not exposing the nesting structure to the Html
-- type-class. This restricts the content of preformated elements to plain
-- text. We could circumvent this by rendering an inner html block and
-- inserting it as unescpaed text. However, I'm not sure if such a solution is
-- required.
newtype Indented h = Indented { indent :: Int -> (h -> h) -> (h -> h) -> h }

-- | Indent a Html document.
indented :: Html h => Indented h -> h
indented h = indent h 0 id id

instance (Html h, Monoid h) => Monoid (Indented h) where
  mempty          = Indented $ \ _ _ _ -> mempty
  h1 `mappend` h2 = Indented $ \n std txt -> std $
    -- FIXME: Should check
    indent h1 n id id `mappend` indent h2 n (unescapedText "\n" `mappend`) id

instance Html h => Html (Indented h) where
    separate h1 h2 = Indented $ \n std txt ->
      indent h1 n std txt `mappend`
      -- add an extra space if no indenting is done because the carriage return
      -- does not count as whitespace.
      (if n == 0 then unescapedText " \n" else unescapedText "\n") `mappend`
      spaces n `mappend` indent h2 n std txt
      
    unescapedText t = Indented $ \_ _ txt -> txt $ unescapedText t
    leafElement   t = Indented $ \_ std _ -> std $ leafElement t

    nodeElement t inner = Indented $ \n std txt -> std $
      (unescapedText (replicate n " ")) `mappend`
      -- FIXME: make sure that computation is forced.
      nodeElement t (indent inner (n+2) 
          (\h -> unescapedText "\n" `mappend` h
                                    `mappend` unescapedText "\n"
                                    `mappend` spaces n)
          (\h -> h))

    modifyAttributeModifier mod h = Indented $ \n std txt ->
      modifyAttributeModifier mod (indent h n std txt)

{- THIS is a simpler instance that indents only 
 
instance Html h => Html (Indented h) where
    separate h1 h2 = Indented $ \n ->
      indent h1 n `mappend`
      -- add an extra space if no indenting is done because the carriage return
      -- does not count as whitespace.
      (if n == 0 then unescapedText " \n" else unescapedText "\n") `mappend`
      unescapedText (replicate n " ") `mappend` indent h2 n
      
    unescapedText = Indented . const . unescapedText
    leafElement   = Indented . const . leafElement

    nodeElement t inner = Indented $ \n ->
      (unescapedText (replicate n " ")) `mappend`
      nodeElement t (unescapedText "\n" `mappend` indent inner (n+2) 
                                        `mappend` unescapedText "\n"
                                        `mappend` unescapedText (replicate n " "))

    modifyAttributeModifier mod h = Indented $ \n ->
      modifyAttributeModifier mod (indent h n)
-}
