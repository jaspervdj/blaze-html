{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, Rank2Types #-}
import Prelude hiding (head)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Exts (IsString(..))
import Data.Monoid
import Data.Binary.Builder
import Control.Monad (liftM)
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

infixr 3 <> 
infixr 3 <->

-- Author: Simon Meier, March 31th, 2010
----------------------------------------

{- Introduction:

This document highlights presents an improved design over what I proposed
before the start of the ZuriHac. The improvements are as follows.

  1. The typeclass can be used both with ordinary Haskell Strings and
     Data.Text. The library user has to make the decision of when to use
     what representation for a unicode stream.

  2. The problem of how to translate a generic html document correctly to
     a bytestream encoded using a partial encoding scheme is adressed.
     Moreover, the design even protects the user from forgetting to put
     the necessary encoding tag.

  3. The difference between a full blown tree representation and a minimal
     html representation is made explicit by writing out both variants and
     showing how to translate the full representation into the minimal
     representation.

My suggestion for further completion is that, we complete all designs of
interest in a single file with a set of example combinators and an expressive
enough example. This way we get a much better overview over the design
decisions that are involved.

-}


-----------------------------------------------------------------------------
-- A type-class for abstracting Html documents
-----------------------------------------------------------------------------

class Monoid h => Html h where
    -- | Character to be converted
    --
    -- NOTE: I'm not sure if this is well-defined as unicode characters are of
    -- unbounded width in general. Perhaps we should only offer the Text
    -- interface.
    --   => Investigate what a Haskell 'Char' is and decide this matter!
    unescapedChar    :: Char -> h
    -- | Text to be converted to html
    unescapedText    :: Text -> h
    -- | Left html, right html
    separate         :: h -> h -> h
    -- | Tag
    leafElement      :: h -> h
    -- | Tag, inner html
    nodeElement      :: h -> h -> h
    -- | Key, value, html taking attributes
    addAttribute     :: h -> h -> h -> h



-----------------------------------------------------------------------------
-- A minimal type-class for abstracting Html documents
-----------------------------------------------------------------------------


-- The above typeclass makes it possible to have a pretty printing interpreter
-- as well as to output in some binary XML formats. I (Simon) like this
-- property. However, let's see what complexity we are adding with respect to a
-- minimal Html typeclass. 
--
-- The class below captures (as far as I can see) the minimal set of combinators
-- required to describe Html documents that are built from unicode chars and
-- support adding up attributes and using them at several places.

class Monoid h => MinimalHtml h where
    -- | Character to be converted
    unescapedCharM   :: Char -> h
    -- | Prepend the current attributes to the given "inner" html that is
    -- evaluated with cleared attributes.
    attributesM      :: h -> h
    -- | attribute and "inner" html taking attributes
    addAttributeM    :: h -> h -> h


-- The 'MinimalHtml' typeclass is expressive enough to emulate the standard
-- 'Html' typeclass. Looking at this emulation, we will see what decisions
-- we fix when using the minimal typeclass.


-- | We need to tag it with a newtype for giving a valid instance
data EmulateHtml h = EH { runEH :: h }


instance Monoid h => Monoid (EmulateHtml h) where
    mempty          = EH mempty
    h1 `mappend` h2 = EH $ runEH h1 `mappend` runEH h2

instance MinimalHtml h => Html (EmulateHtml h) where
    unescapedChar    = EH . unescapedCharM
    unescapedText    = EH . mconcat . map unescapedCharM . T.unpack
    h1 `separate` h2 = EH $ mconcat [runEH h1, unescapedCharM ' ', runEH h2]
    leafElement tag  = EH $ mconcat 
        [ unescapedCharM '<'
        , runEH tag
        , attributesM mempty
        , unescapedCharM '/'
        , unescapedCharM '>'
        ]
    nodeElement tag inner = EH $ mconcat
        [ unescapedCharM '<'
        , runEH tag
        , attributesM $ mconcat 
            [ unescapedCharM '>'
            , runEH inner 
            ]
        , unescapedCharM '<'
        , unescapedCharM '/'
        , runEH tag 
        , unescapedCharM '>'
        ]
    addAttribute key value h = EH $ 
        addAttributeM
          (mconcat
              [ unescapedCharM ' '
              , runEH key
              , unescapedCharM '='
              , unescapedCharM '"'
              , runEH value
              , unescapedCharM '"'
              ]
          )
          (runEH h)

-- I (Simon) don't like that we fix ourself to a concrete representation for
-- attributes and tags. I can very well imagine that other formats could also
-- be interesting and worthwile (e.g. pretty printing and binary xml).


------------------------------------------------------------------------------
-- A Builder Renderer
------------------------------------------------------------------------------

-- Here I demonstrate how to use 'Data.Binary.Builder' to implement an
-- efficient conversion from the internal representation as calls to methods of
-- the 'Html' typeclass to an encoded bytestream represented as a lazy
-- ByteString.
--
-- The conversion is parameterized over the concrete encoding to be used.
-- Currently we only parameterize over the 'Char' encoder. This is subject to
-- change.
newtype HtmlByteString = HB { runHB :: (Char -> Builder) -> Builder -> Builder }

instance Monoid HtmlByteString where
    mempty          = HB $ \_ _    -> mempty
    h1 `mappend` h2 = HB $ \enc as -> 
        runHB h1 enc as `mappend` runHB h2 enc as

instance Html HtmlByteString where
    unescapedChar c = HB $ \enc _ -> enc c
    -- here we should be passing also a special text encoder or use an encoding
    -- function from Data.Text parametrized over the char encoder.
    unescapedText t = HB $ \enc _ -> mconcat . map enc . T.unpack $ t
    h1 `separate` h2 = HB $ \enc as -> mconcat
        [ runHB h1 enc as 
        , enc ' '
        , runHB h2 enc as
        ]
    leafElement tag = HB $ \enc as -> mconcat
        [ enc '<'
        , runHB tag enc mempty
        , as
        , enc '/'
        , enc '>'
        ]
    nodeElement tag inner = HB $ \enc as -> mconcat
        [ enc '<'
        , runHB tag enc mempty
        , as
        , enc '>'
        , runHB inner enc mempty
        , enc '<'
        , enc '/'
        , runHB tag enc mempty
        , enc '>'
        ]
    addAttribute key value h = HB $ \enc as -> 
        runHB h enc $ mconcat
            [ enc ' '
            , runHB key enc mempty
            , enc '='
            , enc '"'
            , runHB value enc mempty
            , enc '"'
            ] `mappend` as

-- NOTE that there are two alternatives for providing this typeclass instance:
--
--   1. We could implement `MinimalHtml` and use the `EmulateHtml` transformer
--      to provide the implementations for the `Html` typeclass.
--
--   2. We could ignore all concrete encodings of the tags and instead insert
--      them using a `Html` typeclass transformer. This way different
--      implementations of how to render tags could share the same underlying
--      bytestring renderer.
--
--
-- Obviously, once we do have concrete benchmarks, then we have to see how much
-- the abstraction penalty costs us. If it is too expensive then it is simple
-- to just fix the architecture afterwards. The otherway round would not work
-- equally well. Especially, as we could not even measure the cost of
-- abstraction.


------------------------------------------------------------------------------
-- Example combinators built ON TOP of the Html class
------------------------------------------------------------------------------

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

(<->) :: Html h => h -> h -> h
(<->) = separate

separated :: Html h => [h] -> h
separated = foldr (<->) mempty

unescapedString :: Html h => String -> h
unescapedString = mconcat . map unescapedChar

unescapedShow :: (Show a, Html h) => a -> h
unescapedShow = unescapedString . show

-- | This is the 'html' tag without an explicit encoding tag. Below the
-- combinator with an explicit encoding can be found.
encodingImplicitHtml :: (Html h) => h -> h
encodingImplicitHtml = nodeElement (unescapedString "html")

body :: (Html h) => h -> h
body = nodeElement (unescapedString "body")

head :: (Html h) => h -> h
head = nodeElement (unescapedString "head")

h1 :: (Html h) => h -> h
h1 = nodeElement (unescapedString "h1")

href :: Text -> Attribute Url
href = attribute "href" . Url

img :: (Html h) => h
img = leafElement (unescapedString "img")

p :: (Html h) => h -> h
p = nodeElement (unescapedString "p")

em :: (Html h) => h -> h
em = nodeElement (unescapedString "em")


-----------------------------------------------------------------------------
-- A Html transformer adding the feature of using an arbitrary encoding tag
-----------------------------------------------------------------------------

newtype EncodingExplicit h = EE { runEE :: h -> h }

instance Monoid h => Monoid (EncodingExplicit h) where
    mempty          = EE $ const mempty
    h1 `mappend` h2 = EE $ \eTag -> runEE h1 eTag `mappend` runEE h2 eTag

instance Html h => Html (EncodingExplicit h) where
    unescapedChar = EE . const . unescapedChar
    unescapedText = EE . const . unescapedText
    h1 `separate` h2 = EE $ \eTag -> runEE h1 eTag `separate` runEE h2 eTag
    leafElement tag = EE $ \_ -> 
        leafElement (runEE tag mempty)
    nodeElement tag inner = EE $ \eTag -> 
        nodeElement (runEE tag mempty) (runEE inner eTag)
    addAttribute key value h = EE $ \eTag ->
        addAttribute (runEE key mempty) (runEE value mempty) (runEE h eTag)

-- Let's see the benefit of this construction
---------------------------------------------

-- | This is the placeholder for an encoding tag.
encodingTag :: EncodingExplicit h
encodingTag = EE id

-- | The standard html combinator constructs an encoding explicit document
-- from a html document for the head and one for the body. The encoding 
-- tag is put as the first tag of the head.
--
-- NOTE This needs to be refined to support setting attributes for the
-- html, head, and body tag.
html :: Html h => h -> h -> EncodingExplicit h
html hd b = encodingImplicitHtml $
    (head $ encodingTag <> EE (const hd)) <> EE (const (body b))

-- | Now we can render an encoding explicit document using the HtmlByteString
-- renderer.
--
-- The type signature reminds us that we need to put the encodingTag somewhere.
-- However this is not enforced, as any html document could be handed over
-- here.  The enforcment happens only indirectly in the form of the combinators
-- presented to the library user.
htmlUtf8 :: EncodingExplicit HtmlByteString -> BL.ByteString
htmlUtf8 h = 
  toLazyByteString $ runHB (runEE h utf8tag) encodeCharUtf8 mempty
  where
    utf8tag = unescapedText 
       "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>"


-- The following functions need to be implemented directly and more
-- efficiently. They are the working horses.
--
-- The implementations here just serve to get a functional example.
textToB :: Text -> Builder
textToB = fromByteString . encodeUtf8

stringToB :: String -> Builder
stringToB = textToB . T.pack

encodeCharUtf8 :: Char -> Builder
encodeCharUtf8 = stringToB . return


    
------------------------------------------------------------------------------
-- Escaping characters that would otherwise be parsed wrongly
------------------------------------------------------------------------------

-- Apart from the escaping that needs to be done to encode the unicode
-- character stream correctly as a stream of bytes in some format, we also
-- need to take care that all characters are interpreted correctly by the
-- reading Html parser.
--
-- This escaping is handled ON TOP of the Html class. The concrete combinator
-- libraries for a document ensure that all values are correctly escaped. Quite
-- a bit of this escaping can be done using the corresponding unicode
-- characters.

-- | A dummy implementation. Actually the char is inspected and either several
-- unescapedChar's are mconcatenated in the Html monad or a corresponding
-- unescapedText is used for the Html entity.
char :: Html h => Char -> h
char = unescapedChar

-- | A dummy implementation. Actually this needs to be implemented such that it
-- makes use of the internal representation of 'Text' in order to gain maximal
-- speed. Depending on the requirements here a more precise interface for
-- injecting unicode characters into a Html document will be required.
text :: Html h => Text -> h
text = unescapedText

-- | Convert a string to a Html document.
string :: Html h => String -> h
string = mconcat . map char

-- | Convert a showable value to a Html document. Will be renamed to 'show'
-- once it is in the right module.
showH :: (Show a, Html h) => a -> h
showH = string . show
    
------------------------------------------------------------------------------
-- Setting attributes both for leaf and node elements using the same operator
------------------------------------------------------------------------------

-- Here the construction for the (!) operator should be repeated. 
--
-- Special care has to be taken w.r.t. the interaction with the IsString class.
--
-- One way out (which I like) is that only the concrete attribute combinators
-- are required to work without type annotations. In the other case, the library
-- user should anyways know very well what he is doing.
--
-- The namespace qualification A. for attributes and H. for documents makes such
-- a route quite viable.


-- Use cases:
--
--    leafElement "blah" ! attr
--    leafElement "blah" ! [attr]
--    nodeElement "blah" ! attr
--    nodeElement "blah" ! [attr]

instance Html h => Html (h -> h) where
  -- Simon: Think about throwing errors in all cases except addAttribute.
  unescapedChar c  = const $ unescapedChar c
  unescapedText t  = const $ unescapedText t
  h1 `separate` h2 = const $ h1 mempty `separate` h2 mempty
  leafElement tag  = const $ leafElement (tag mempty)
  nodeElement tag inner = const $ nodeElement (tag mempty) (inner mempty)
  addAttribute key value node inner = 
    addAttribute (key mempty) (value mempty) (node inner) 

-- NOTE: I'm not sure if this is any good. I tend to discourage support for
-- creating attributes explicitly. Concrete combinators should be used as they
-- guarantee the correct escaping.

class UnescapedData a where
  unescapedData :: Html h => a -> h

instance UnescapedData Text where
  unescapedData = unescapedText

instance UnescapedData Char where
  unescapedData = unescapedChar

instance UnescapedData a => UnescapedData [a] where
  unescapedData = mconcat . map unescapedData

instance UnescapedData Int where
  unescapedData = unescapedData . show

class Attributable a where
  (!) :: Html h => h -> a -> h

instance UnescapedData a => Attributable (a, a) where
  h ! (key, value) = addAttribute (unescapedData key) (unescapedData value) h

instance Attributable a => Attributable [a] where
  h ! attrs = foldl' (!) h attrs


-- Escaped attribute values
---------------------------

-- NOTE: Here the escaping should possibly be a bit different for the default
-- case.

class AttributeValue a where
  attributeValue :: Html h => a -> h

instance AttributeValue Text where
  attributeValue = text

instance AttributeValue a => AttributeValue [a] where
  attributeValue = mconcat . map attributeValue

instance AttributeValue Char where
  attributeValue = char

-- Here we can save some energy as show for strings will always produce
-- properly escaped values.
instance AttributeValue Int where
  attributeValue = unescapedString . show

-- will be escaped according to URL escaping rules.
newtype Url = Url { getUrl :: Text }

instance AttributeValue Url where
  attributeValue = escapeUrl . getUrl
    where
    -- dummy escaping
    escapeUrl = text

data Attribute a = Attribute !Text !a

instance AttributeValue a => Attributable (Attribute a) where
  h ! (Attribute key value) = 
    addAttribute (unescapedText key) (attributeValue value) h

attribute :: Text -> a -> Attribute a
attribute key value = Attribute key value

idA :: a -> Attribute a
idA = attribute "id" 

idA' :: Text -> Attribute Text
idA' = idA


newtype Attrib h = Attrib (h -> h)

-- testAttrib :: Html h => Attrib h
-- testAttrib = Attrib "blah" (unescapedString "world")

-- instance Attributable Attrib where
  -- h ! (Attrib key value) = addAttribute (unescapedText key) value

-- does not really work because inferred types are too general
class Html h => Attributable2 a h where
  (<!) :: h -> a -> h

instance (AttributeValue a, Html h) => Attributable2 (a,a) h where
  h <! (k,v) = addAttribute (attributeValue k) (attributeValue v) h

instance (Attributable2 a h) => Attributable2 [a] h where
  h <! attrs = foldl' (<!) h attrs

instance Html h => Attributable2 (Attrib h) h where
  h <! (Attrib f) = f h

-- the whole fuss because we want to save a single operator?

attrib :: Html h => Text -> h -> Attrib h
attrib key value = Attrib $ addAttribute (unescapedText key) value

href2 :: Html h => Text -> Attrib h
href2 = attrib "href" . text  -- should be URL escaping

id2 :: (Html h, AttributeValue a) => a -> Attrib h
id2 = attrib "id" . attributeValue


class Attributable3 a where
  (#!) :: Html h => h -> a h -> h


-- Hmm, now we lost the ability to make lists of attributes an instance.
-- instance (Attributable3 a) => Attributable3 ??? ([Attrib a]) where
--   ???

instance Attributable3 Attrib where
  h #! (Attrib f) = f h


-- QUINTESSENCE

-- I would go for two separate operators now one for adding lists of
-- attributes (<!) and one for adding a single attribute (!). Attributes
-- are just functions from 'h -> h' wrapped in a newtype.

-- Then we can ask the mailing list if they have a nice solution for collapsing
-- the two operators into one without giving up functionality.

-- NOTE the reason for using 'Attrib h' as a type instead of '(Text,Text)' is
-- that the escaping needs to be done directly on the Html layer in order to
-- avoid expensive intermediate 'Text's for escaped attribute values. The
-- decision of how to do escaping lies with the attribute generator and not the
-- consumer.
--
-- Having this polymorphic 'h' in there is the origin of all trouble.

-- It is an open question whether polymorphic variants of the attribute
-- values are required. Due to the different escaping that is needed for
-- different attribute values I tend to rather go for the simple solution that
-- each attribute 'att' has the form
--
--      att :: Html h => Text -> h
--      att' :: Html h => String -> h
--
--    and for names clashing with Haskell keywords (e.g. class) it would be
--
--      class_ :: Html h => Text -> h
--      class' :: Html h => String -> h
--
-- But I'm not yet sure. If we can make a good design where the conversion
-- semantics for an attribute is always clear, then I would be in for a
-- polymorphic attribute setting variant along the lines of
-- 'AttributeValue'. Obviously, we would still have to provide a binding
-- that has its type fixed to 'Text' or 'String' as otherwise overloaded
-- string literals lead to too generic types.
--
--   An example for polymorphic attribute values. 
--
--      att :: (Html h, AttributeValue a) => a -> h
--      att' :: Html h => Text -> h
--
--   It seems as if this construction fixes the escaping to be used to the one
--   chosen in the instances of the 'AttributValue' class. One way out would be
--   to use separate typeclasses for every relevant escaping. This could even
--   be viable as there are few such escapings and for most of them the actual
--   implementation can be shared.


------------------------------------------------------------------------------
-- Html Monad
------------------------------------------------------------------------------

newtype HtmlMonad h a = HtmlMonad { runHtmlMonad :: h }

instance (Monoid h) => Monoid (HtmlMonad h a) where
    mempty                        = HtmlMonad mempty
    mappend (HtmlMonad h1) (HtmlMonad h2) = HtmlMonad $ h1 `mappend` h2

instance (Html h) => Html (HtmlMonad h a) where
    separate (HtmlMonad h1) (HtmlMonad h2) = HtmlMonad $ h1 `separate` h2
    unescapedChar c = HtmlMonad $ unescapedChar c
    unescapedText t = HtmlMonad $ unescapedText t
    leafElement (HtmlMonad t) = HtmlMonad $ leafElement t
    nodeElement (HtmlMonad t) (HtmlMonad h) = HtmlMonad $ nodeElement t h
    addAttribute (HtmlMonad k) (HtmlMonad v) (HtmlMonad h) =
        HtmlMonad $ addAttribute k v h
    
instance (Monoid h) => Monad (HtmlMonad h) where
    return   = mempty
    (HtmlMonad h1) >> (HtmlMonad h2) = HtmlMonad $ h1 `mappend` h2
    (HtmlMonad h1) >>= f = let HtmlMonad h2 = f errorMessage
                           in HtmlMonad $ h1 `mappend` h2
      where
        errorMessage = error "HtmlMonad: >>= returning values not supported."

instance Html h => IsString (HtmlMonad h a) where
    fromString = string

-----------------------------------------------------------------------------
-- An example document
-----------------------------------------------------------------------------

-- | NOTE that most document fragments are unaware of the explicit encoding
-- tag. They work both with and without this tag.
testBody :: Html h => h
testBody = runHtmlMonad $ do
    h1 $ unescapedText "BlazeHtml"
    img ! href "logo.png" ! idA (1::Int)
    p ! idA' "main" $ separated
        [ "is a"
        , (em $ unescapedText "blazingly")
        , "fast HTML generation library."
        , "Note that it also handles unicode: └─╼ "
        ]

-- | However the 'html' combinator will always build an encoding explicit
-- document with the encodingTag put as the first tag of the head.
testDoc :: Html h => EncodingExplicit h
testDoc = html mempty testBody

-- NOTE that the the conversion below is a bit complicated (and would be slow
-- for large data) because Data.Text does not support decoding from a
-- lazy-bytestring.
testText :: Text
testText = decodeUtf8 . mconcat . BL.toChunks $ htmlUtf8 testDoc

-- To see that the UTF-8 roundtrip works we have to output the text as
-- otherwise the characters get escaped.
testTextIO :: IO ()
testTextIO = T.putStrLn testText


