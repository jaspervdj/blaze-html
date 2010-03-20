{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Text.BlazeHtml.Internal.Html
    ( Attributes
    , Html (..)
    , setUnescapedAttributes
    , addUnescapedAttribute
    , addUnescapedAttributes
    , clearAttributes
    , IsAttributable((!))
    , (<!)
    ) where

import Data.Monoid
import Text.BlazeHtml.Text (Text)

-- | Attributes as an association list. 
--   Please do not rely on the fact that this is an association list - this is
--   subject to change.
type Attributes = [(Text,Text)]

-- | Function that manipulates attributes. This is used for the CPS.
type AttributeManipulation = Attributes -> Attributes

-- | Any Html document is a monoid. Furthermore, the following equalities hold.
--
--    renderUnescapedText mempty = mempty
--
--    renderUnescapedText t1 `mappend` renderUnescapedText t2 = renderText (t1 `mappend` t2)
--
--    setAttributes a (renderUnescapedText t) = renderUnescapedText t
--
--    addAttributes a (renderUnescapedText t) = renderUnescapedText t
--
--    setAttributes a1 (setAttributes a2 h) = setAttributes a2 h
--
--    addAttributes a1 (setAttributes a2 h) = setAttributes a2 h
--
--    addAttributes a1 (addAttributes a2 h) = addAttributes (a2 `mappend` a1) h 
--
--
--    modifyUnescapedAttributes f (t1 `mappend` t2) = 
--    modifyUnescapedAttributes t1 `mappend` modifyUnescapedAttributes t2
--
--    modifyUnescapedAttributes f (modifyUnescapedAttributes g h) = modifyUnescapedAttributes (g.f) h
--
--    modifyUnescapedAttributes f (renderUnescapedText t) = renderUnescapedText t
--
--    renderElement t h = renderElement t (modifyUnescapedAttributes (const []) h)
--
--  Note that the interface below may be extended, if a performing
--  implementation requires it.
--
class Monoid h => Html h where
    -- | Render text -- no escaping is done.
    renderUnescapedText       :: Text -> h
    -- | Render a leaf element with the given tag name.
    renderLeafElement         :: Text -> h
    -- | Render an element with the given tag name and the given inner html.
    renderElement             :: Text -> h -> h
    -- | Set the attributes of the outermost element.
    modifyUnescapedAttributes ::
        (AttributeManipulation -> AttributeManipulation) -> h -> h

-- | Set the attributes all outermost elements to the given list of
-- unescaped HTML attributes.
setUnescapedAttributes :: (Html h) => Attributes -> h -> h
setUnescapedAttributes = modifyUnescapedAttributes . (.) . const

-- | Add a single unescaped HTML attribute to all outermost elements.
--
-- > addAttribute "src" "foo.png"
addUnescapedAttributes :: (Html h) => Attributes -> h -> h
addUnescapedAttributes = modifyUnescapedAttributes . (.) . (++)

-- | Add a single HTML attribute to all outermost elements.
--
-- > addAttribute "src" "foo.png"
addUnescapedAttribute :: (Html h) => Text -> Text -> h -> h
addUnescapedAttribute key value =
    modifyUnescapedAttributes (((key, value) :) .)

-- | Remove the HTML attributes of all outermost elements.
clearAttributes :: (Html h) => h -> h
clearAttributes = setUnescapedAttributes []

(<!) :: (IsAttributable a, Html h) => (h -> h) -> a -> h -> h
el <! a = \inner -> (el inner) ! a


class IsAttributable q where
    (!) :: Html h => h -> q -> h

instance IsAttributable (Text,Text) where
    e ! attr = addUnescapedAttributes [attr] e

instance IsAttributable [(Text,Text)] where
    e ! attrs = addUnescapedAttributes attrs e
