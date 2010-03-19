module Text.BlazeHtml.Internal.Html
    ( Attributes
    , Html (..)
    , addAttributes
    , setAttributes
    ) where

import Data.Monoid
import Data.Text (Text)

-- | Attributes as an association list. 
--   Please do not rely on the fact that this is an association list - this is
--   subject to change.
type Attributes = [(Text,Text)]

-- | Any Html document is a monoid. Furthermore, the following equalities hold.
--
--    renderText mempty = mempty
--    renderText t1 `mappend` renderText t2 = renderText (t1 `mappend` t2)
--
--    modifyAttributes f (modifyAttributes g h) = modifyAttributes (g.f) h
--    modifyAttributes f (renderText t) = renderText t
--    renderElement t h = renderElement t (modifyAttributes (const []) h)
--
--  Note that the interface below may be extended, if a performing
--  implementation requires it.
--
class Monoid h => Html h where
    -- | Render text -- no escaping is done.
    renderUnescapedText :: Text -> h
    -- | Render a leaf element with the given tag name.
    renderLeafElement   :: Text -> h
    -- | Render an element with the given tag name and the given inner html.
    renderElement       :: Text -> h -> h
    -- | Set the attributes of the outermost element.
    modifyAttributes    :: (Attributes -> Attributes) -> h -> h

addAttributes :: (Html h) => Attributes -> h -> h
addAttributes attributes = modifyAttributes (++ attributes)

setAttributes :: (Html h) => Attributes -> h -> h
setAttributes attributes = modifyAttributes (const attributes)
