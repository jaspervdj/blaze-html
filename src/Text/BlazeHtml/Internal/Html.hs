{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Text.BlazeHtml.Internal.Html
    ( Attribute
    , Html (..)
    , setUnescapedAttributes
    , addUnescapedAttribute
    , addUnescapedAttributes
    , clearAttributes
    , text
    ) where

import Data.Monoid

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Escaping

infixl 2 !

-- | Attribute as a tuple 
-- Please do not rely on the fact that this is an association list - this is
-- subject to change.
type Attribute = (Text,Text)

-- | A class for types you can add as an Attribute to HTML elements.
class AttributeList a where
    toAttributeList :: a -> [Attribute]

-- | You can add a single attribute to an HTML element.
instance AttributeList Attribute where
    toAttributeList = return

-- | You can add a list of attributes to an HTML element.
instance AttributeList [Attribute] where
    toAttributeList = id

-- | Function that manipulates attributes. This is used for the CPS.
type AttributeManipulation = [Attribute] -> [Attribute]

-- | Any Html document is a monoid. Furthermore, the following equalities hold.
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
--  Note that the interface below may be extended, if a performing
--  implementation requires it.
--
class (Monoid h) => Html h where
    -- | Render text -- no escaping is done.
    unescapedText    :: Text -> h
    -- | Render a leaf element with the given tag name.
    leafElement      :: Text -> h
    -- | Render an element with the given tag name and the given inner html.
    nodeElement      :: Text -> h -> h
    -- | Set the attributes of the outermost element.
    modifyAttributes ::
        (AttributeManipulation -> AttributeManipulation) -> h -> h

    -- | An operator to add attributes. This has a sensible default
    -- implementation.
    (!) :: (AttributeList l) => h -> l -> h    
    el ! attr = addUnescapedAttributes (toAttributeList attr) el
    
    
-- | Set the attributes all outermost elements to the given list of
-- unescaped HTML attributes.
setUnescapedAttributes :: (Html h) => [Attribute] -> h -> h
setUnescapedAttributes = modifyAttributes . (.) . const

-- | Add a single unescaped HTML attribute to all outermost elements.
--
-- > addAttribute "src" "foo.png"
addUnescapedAttributes :: (Html h) => [Attribute] -> h -> h
addUnescapedAttributes = modifyAttributes . (.) . (++)

-- | Add a single HTML attribute to all outermost elements.
--
-- > addAttribute "src" "foo.png"
addUnescapedAttribute :: (Html h) => Text -> Text -> h -> h
addUnescapedAttribute key value = modifyAttributes (((key, value) :) .)

-- | Remove the HTML attributes of all outermost elements.
clearAttributes :: (Html h) => h -> h
clearAttributes = setUnescapedAttributes []

-- | Create an 'Html' value from a chunk of text, with proper string escaping.
text :: (Html h) => Text -> h
text = unescapedText . escapeHtml
 
instance (Html b) => Html (a -> b) where
    unescapedText txt _       = unescapedText txt
    leafElement   txt _       = leafElement txt   
    nodeElement   txt  fn val = nodeElement txt $ fn val
    modifyAttributes f fn val = modifyAttributes f $ fn val
        
    fn ! attr = \arg -> fn arg ! attr
