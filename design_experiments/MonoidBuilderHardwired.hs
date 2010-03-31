{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Binary.Builder
import Control.Monad (liftM)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

{-

Hi Japser,

your solution is interesting. As far as I see it, it represents exactly the
fast path we wanted to implement. It takes care of the encoding Problem and
escaping can also be done efficiently.

Compared to the solution using the `Html` typeclass. We lose any ability to
inspect the tree structure. What we gain is a simpler implementation with less
indirection.

In my opinion, having this tree structure explicitly is a good thing for the
following reasons.

  1. It allows for different output formats for the tags; e.g. binary XML,
     and various forms of pretty printing.

  2. One can imagine doing more complicated analyses on the the document
     without having to parse it anymore.

In my opinion, the first point is important. It was the reason for implementing
the Html typeclass in the first place. The second point is not so important but
nice to have. Especially as it may lead to other people having cool ideas what
one could do with these html documents.

Hence, I suggest that we make sure that the first library implementation does
not lead to incompatible code on the client side, when later changing to a
more expressive typeclass allowing for more inspection of the tree structure.

The easiest way to achieve this is to already use the `Html` typeclass ;-)
Let's revisit it and see, if we could shrink it:

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

For pretty-printing the structure provided by 
 
   mempty, mappend, separate, unescapedText, leafElement, nodeElement

is essential. We need to be able to differentiate between them in order to make
the right indenting/line-breaking decisions.

For setting/adding attributes the `modifyAttributeModifier` method is essential.

The only thing, we could get rid of (and I'd be VERY happy to get rid of it) is
the `addAttributable` method.

-------

There is also a different perspective for comparing our two approaches. Namely
by making explicit the assumptions they make about the domain of interest.

  1. Both approaches assume all data in a Html document is of type Text.

  2. Both approaches assume that Html documents are monoids.

Your approach additionally assumes that

  3. The tree structure of a Html documents will always be inspected in the
     SAME way. (This assumptions comes from your fixed implementation.)

  4. Every Html document needs a placeholder for an encodingTag.

  5. All additional information required for describing an Html document
     will be threaded through its underlying monoid; i.e. it cannot make
     use of the structure of a Html document.

I think that assumptions 3., 4., and 5. are too strong.

Assumption 3. violates the desire to support different output formats like
Binary XML or pretty printing. 

I would not make assumption 4. because there may be documents or especially
document fragments that do not need to know about an encoding tag. However, it
is no strong assumption as a component not relying on it will not break.
Compared to the solution of using `Html h => UnencodedHtml h` for a document
that does need an encoding tag we only loose the explicit type information that
an encoding tag occurs.

Assumption 5. is similar to assumption 3. It stems from your fixing of the `Html`
type constructor. I currently, cannot tell how bad it is. At least any special
dealing with attributes is excluded.


The assumption I am most unsure about is assumption 1., which both approaches
make. Somehow, it feels strange that we have to restrict ourselves to a fixed
representation for the input data.

Couldn't we get rid of it using the following construction:


class HtmlData a h where
    unescapedHtml :: a -> h

class Monoid h => Html h where
    separate         :: h -> h -> h
    leafElement      :: h -> h
    nodeElement      :: h -> h -> h
    modifyAttributes :: (h -> h) -> h -> h


-}

data Renderer m = Renderer
    { render      :: Text -> m
    , encodingTag :: Html m
    }

type Attribute = (Text, Text)

newtype Html m = Html (ReaderT [Attribute] (Reader (Renderer m)) m)

instance Monoid m => Monoid (Html m) where
    mempty  = Html $ return mempty
    mappend (Html h1) (Html h2) = Html $ do
        m1 <- h1
        m2 <- h2
        return $ m1 `mappend` m2

-- | Simple helper function to render the attributes.
attributes :: (Monoid m) => Html m
attributes = Html $ do
    attrs <- ask 
    case attrs of
        []    -> return mempty
        attrs -> do renderer <- lift ask
                    return $ mconcat $ map (attribute $ render renderer) attrs
  where
    attribute render' (k, v) = mconcat $ map render' [" ", k, "=\"", v, "\""]
                    
unescapedText :: (Monoid m) => Text -> Html m
unescapedText text = Html $ do renderer <- lift ask
                               -- return $ render renderer mempty
                               -- SM: I assume the above line should be:
                               return $ render renderer text

leafElement :: (Monoid m) => Text -> Html m
leafElement text = mconcat [ unescapedText "<"
                           , unescapedText text
                           , attributes
                           , unescapedText "/>"
                           ]

nodeElement :: (Monoid m) => Text -> Html m -> Html m
nodeElement tag inner = mconcat [ unescapedText "<"
                                 , unescapedText tag
                                 , attributes
                                 , unescapedText ">"
                                 , modifyAttributes (const []) inner
                                 -- SM: Added the closing tag
                                 , unescapedText "</"
                                 , unescapedText tag
                                 , unescapedText ">"
                                 ]

modifyAttributes :: (Monoid m) => ([Attribute] -> [Attribute]) -> Html m -> Html m
modifyAttributes f (Html h) = Html $ local f h

-- What follows is a fast utf8 renderer renderer
utf8Renderer :: Renderer Builder
utf8Renderer = Renderer
    { -- SM: The combination `fromByteString . encodeUtf8` is what we should replace by
      --     a direct implementation using putWordXXX from Data.Binary.Builder. The reason
      --     is that the chunks we get from Data.Text.encodeUtf8 are too small
      --     and they are NOT compacted. Compaction is required to make a
      --     scattered write efficient as otherwise too many pointers have to
      --     be transferred.
      render       = fromByteString . encodeUtf8
    , -- Hardcoded for now
      encodingTag = unescapedText "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
    }
