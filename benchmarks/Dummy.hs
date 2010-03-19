{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
-- | Another draft of an API for a blazingly fast Html generation library.
--
-- Here we take the following approach. 
--
--   (1) We define a Html typeclass for the basic combinators required to build
--       well-formed Html documents. Part of their semantics is defined by
--       equalities they satisfy.
--
--   (2) Based upon these combinators we build the whole set of combinators
--       required for a convenient specification of Html documents.
--
--   (3) We provide an instance of the Html typeclass that handles the
--       common use of outputting a Html document using some IO action.
--       We make sure to configure the compiler such that all the abstraction
--       penalties are removed.
--
--  Compared to the draft on 
--
--    http://github.com/jaspervdj/BlazeHtml/blob/master/src/Text/BlazeHtml.hs
--
--  this approach has the advantage that users of the library are not tied to
--  the IO monad for using a Html document, while still getting the good speed
--  for the case of outputting the document using IO. A typical case where the
--  user wouldn't want to use the super fast execution path is when debugging
--  the html output. There a pretty printing outputer is more suitable.
--
module ClassyHtml where

import Control.Monad.Reader
import Control.Monad.Writer

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO

infix 1 </
infix 1 <!
infixl 1 !

------------------------------------------------------------------------------
-- Html Typeclass
------------------------------------------------------------------------------

-- | Attributes as an association list. 
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
  --
  -- NOTE: A good suggestion for handling escaping is required. I guess its not
  -- utterly difficult, but I haven't had time to think it throug toroughly.
  renderText        :: Text -> h
  -- | Render a leaf element with the given tag name.
  renderLeafElement :: Text -> h
  -- | Render an element with the given tag name and the given inner html.
  renderElement     :: Text -> h -> h
  -- | Set the attributes of the outermost element.
  modifyAttributes  :: (Attributes -> Attributes) -> h -> h


-- Derived combinators
--------------------------

-- | Set the attributes of a Html document.
(!) :: Html h => h -> Attributes -> h
html ! attrs = modifyAttributes (const attrs) html

-- | Set the attributes of an element accepting a list of inner html elements.
(<!) :: Html h => (h -> h) -> Attributes -> [h] -> h
(<!) e attrs inner = e (mconcat inner) ! attrs

-- | Set the inner html to a list of inner html elements
(</) :: Html h => (h -> h) -> [h] -> h
(</) e inner = e $ mconcat inner


-- Concrete Html combinators
----------------------------

em :: Html h => h -> h
em = renderElement "em"

h1 :: Html h => Text -> h
h1 = renderElement "h1" . renderText

img :: Html h => Text -> Text -> h
img src alt = renderLeafElement "img" ! [("src", src), ("alt", alt)]

p :: Html h => h -> h
p = renderElement "p"

-- | Render text such that 'text t1 `mappend` text t2 = mconcat [" ",t1," ",t2]'.
--
-- The above property ensures that words from separate 'text' combinators don't
-- get concatenated in the Html document; c.f. the last paragraph in the
-- example in the source code below.
text :: Html h => Text -> h
text = renderText . (mappend " ")

-- An example for testing
-------------------------

test :: Html h => h
test = mconcat
    [ h1 "ClassyHtml" ! [("id", "header")]
    , img "logo.png" "ClassyHtml logo"
    , p <! [("id","explanation")] $
        [ text "ClassyHtml is a blazingly fast HTML combinator library."
        , em $ text "ClassyHtml is based on a Html typeclass together with \
                     \a set of custom combinators."
        , text "This gives us quite readable code and nice \
                \generalization possibilities."
        ]
    , p </ 
        [ text "Developed by Zack Branigan"
        , text "and Leela Taranga"
        ]
    ]


------------------------------------------------------------------------------
-- An instance of a Html document outputting everything to IO
------------------------------------------------------------------------------

-- | A text outputting function.
type Outputter = Text -> IO ()

-- | This type explains the approach taken in 
--
--    http://github.com/jaspervdj/BlazeHtml/blob/master/src/Text/BlazeHtml.hs
--
-- as a special instance of the Html typeclass. The only difference is that
-- we require setting of attributes to explicitly mention the Html element that
-- should receive these attributes.
newtype HtmlIO = 
  HtmlIO { runHtmlIO :: ReaderT Outputter (ReaderT Attributes IO) () }

-- | Retrieve the outputer from the outer reader and lift it.
getOutputter = (liftIO.) `liftM` ask

-- | Retrieve the attributes from
getAttributes = lift ask

-- | Helper function to render the attributes.
renderAttributes :: Attributes -> Text
renderAttributes [] = T.empty
renderAttributes t  = T.init . foldr append mempty $ t
  where
  append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlIO where
  mempty        = HtmlIO $ return ()
  mappend m1 m2 = HtmlIO $ runHtmlIO m1 >> runHtmlIO m2

instance Html HtmlIO where
  renderText t = HtmlIO $ do
    out <- getOutputter
    out t
  renderLeafElement t = HtmlIO $ do
    out <- getOutputter
    attrs <- getAttributes
    out $ "<" `mappend` t `mappend` " "
    out $ renderAttributes attrs
    out $ "/>"
  modifyAttributes f h = 
    HtmlIO $ ReaderT $ \out -> local f (runReaderT (runHtmlIO h) out)
  renderElement t h = HtmlIO $ do
    out <- getOutputter
    attrs <- getAttributes
    out $ "<" `mappend` t
    out $ renderAttributes attrs
    out $ ">"
    runHtmlIO $ modifyAttributes (const []) h
    out $ "</" `mappend` t `mappend` ">"

-- | Output html to stdout.
renderHtmlIO :: HtmlIO -> IO ()
renderHtmlIO = (`runReaderT` []) . (`runReaderT` IO.putStr) . runHtmlIO

-- The goal is then to optimize the compiler output when using 'renderHtmlIO h'
-- for some 'Html h'.
--
-- This should be achievable using specialize pragmas and probably some
-- inlining and rewritting; e.g.

{-# SPECIALIZE test :: HtmlIO #-}


-----------------------------------------------------------------------------
-- A try for a monadic interface
-----------------------------------------------------------------------------

-- The original draft featured a monadic interface for building Html documents.
-- I don't see a difference in runtime, as appropriate compiler transformations
-- blow away the fixed lists documents are constructed from now.
--
-- However, the monadic interface also allows for a few operators less, which is
-- a desirable property -- provided the semantics stays reasonable.
--
-- There may be a possibility to realize this syntax by instantiating a writer
-- monad appropriatly. However, I did not succeed yet, as type inference comes
-- up with too general types for the applications of the monadic bind. 
--
-- As far as I understand it, the core of the problem is the desire to reuse
-- the combinators which have already been defined for Html documents. If we
-- would introduce separate monadic versions this problem would go away.
--
-- Note that the stuff below is nothing but a half developed draft.

renderDefault :: Writer HtmlIO () -> IO ()
renderDefault = renderHtmlIO . snd . runWriter


type HtmlBuilder h a = Writer h a 

instance Monoid h => Monoid (Writer h ()) where
  mempty        = return ()
  mappend m1 m2 = m1 >> m2

instance Html h => Html (Writer h ()) where
  renderText = tell . renderText
  renderLeafElement =  tell . renderLeafElement
  renderElement t =  tell . renderElement t . snd . runWriter
  modifyAttributes f =  tell . modifyAttributes f . snd . runWriter

data HtmlMonad h a = HtmlMonad () h

instance Html h => Monad (HtmlMonad h) where
    return = HtmlMonad ()

{- The example from the original draft. It doesn't compile yet due to type
 - inference producing too general types.
 
main = renderDefault $ do
    h1 "BlazeHtml" ! [("id", "header")]
    img "logo.png" "BlazeHtml logo"
    p $ do text "BlazeHtml is a blazing fast HTML combinator library."
           em $ text "BlazeHtml uses a monadic interface."
           text "This gives us very readable code."
 -}

