{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
module Text.BlazeHtml.Internal.Html
    ( Attribute
    , Attributable
    , Html (..)
    , setUnescapedAttributes
    , clearAttributes
    ) where

import Data.Monoid

import Text.BlazeHtml.Text (Text)

-- | An attribute as a tuple of a key and a value.
type Attribute = (Text,Text)

-- | A function modifying a list of attributes. This is used for the efficient
-- concatenation of attributes. 
type AttributeModifier = [Attribute] -> [Attribute]

-- | A class for types you can add as an attribute to HTML elements.
--
-- TODO: Decide whether this class should be exported openly; e.g. to allow making
-- Data.Map attributable.
class Attributable a where
     attributeModifier :: a -> AttributeModifier -> AttributeModifier

-- | Adding single attributes to HTML elements. No escaping is done.
instance Attributable Attribute where
    attributeModifier attr = ((attr:) .)

-- | Adding lists of attributes HTML elements. No escaping is done.
instance Attributable [Attribute] where
    attributeModifier attrs = ((attrs++) .)


-- | Any Html document is a monoid. Furthermore, the following equalities hold.
--
--   TODO: Clean up the equalities.
--
--    renderUnescapedText mempty = mempty
--
--    renderUnescapedText t1 `mappend` renderUnescapedText t2 = renderText (t1 `mappend` t2)
--
--    setUnescapedAttributes a (renderUnescapedText t) = renderUnescapedText t
--
--    addUnescapedAttributes a (renderUnescapedText t) = renderUnescapedText t
--
--    setUnescapedAttributes a1 (setUnescapedAttributes a2 h) = setUnescapedAttributes a2 h
--
--    addUnescapedAttributes a1 (setUnescapedAttributes a2 h) = setUnescapedAttributes a2 h
--
--    addUnescapedAttributes a1 (addUnescapedAttributes a2 h) = addUnescapedAttributes (a2 `mappend` a1) h 
--
--    renderElement t h = renderElement t (modifyUnescapedAttributes (const []) h)
--
---------------------------------------------------------------------
--
--    The following need to be tested in a more compreensive way:
--  
--    modifyUnescapedAttributes f (t1 `mappend` t2) = 
--    modifyUnescapedAttributes t1 `mappend` modifyUnescapedAttributes t2
--
--    modifyUnescapedAttributes f (modifyUnescapedAttributes g h) = modifyUnescapedAttributes (g.f) h
--
--    modifyUnescapedAttributes f (renderUnescapedText t) = renderUnescapedText t
--
---------------------------------------------------------------------
--
--  Defining separate
--
--    (h1 `separate` mempty) `separate` h2 = h1 `separate h2
--    h1 `separate` (mempty `separate` h2) = h1 `separate h2
--    (h1 `separate` h2) `mappend`  h3 = h1 `separate (h2 `mappend  h3)
--    (h1 `mappend`  h2) `separate` h3 = h1 `mappend  (h2 `separate h3)
--    (h1 `separate` h2) `separate` h3 = h1 `separate (h2 `separate h3)
--
--    modifyAttributes f (h1 `separate` h2) = 
--    modifyAttributes f h1 `separate` modifyAttributes f h2
--
--
--  Note that the interface below may be extended, if a performing
--  implementation requires it.
--
class (Monoid h) => Html h where
    -- | Compose two html documents separated by whitespace.
    separate                :: h -> h -> h
    separate h1 h2          = h1 `mappend` unescapedText " " `mappend` h2
    -- | Render text witout any escaping.
    unescapedText           :: Text -> h
    -- | Render a leaf element with the given tag name.
    leafElement             :: Text -> h
    -- | Render an element with the given tag name and the given inner html.
    nodeElement             :: Text -> h -> h
    -- | Modify the attributes of the outermost elements.
    modifyAttributeModifier ::
        (AttributeModifier -> AttributeModifier) -> h -> h

    -- | Add an attributable value to the attributes of the outermost element.
    -- Escaping depends on the type of the attributable value. For 'Attribute'
    -- and '[Attribute]' no escaping is done.
    addAttributable         :: Attributable a => a -> h -> h    
    addAttributable attr    = modifyAttributeModifier (attributeModifier attr)
    
-- | Allow attribute setting on nested HTML elements. 
--
-- TODO: 
-- Give instance definition for separate.
--
-- Explain the precise semantics for the individual combinators in this
-- context.
instance (Html h) => Html (h -> h) where
    separate _ _          = error "Html (h -> h): separate not yet defined"
    unescapedText txt _     = unescapedText txt
    leafElement   txt _     = leafElement txt   
    nodeElement             = (.) . nodeElement
    modifyAttributeModifier = (.) . modifyAttributeModifier
    addAttributable attr nest inner = addAttributable attr (nest inner)
    
-- | Set the attributes all outermost elements to the given list of
-- unescaped HTML attributes.
setUnescapedAttributes :: (Html h) => [Attribute] -> h -> h
setUnescapedAttributes = modifyAttributeModifier . (.) . const

-- | Remove the HTML attributes of all outermost elements.
clearAttributes :: (Html h) => h -> h
clearAttributes = setUnescapedAttributes []
