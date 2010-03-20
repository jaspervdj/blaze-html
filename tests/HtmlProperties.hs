{-# LANGUAGE TypeSynonymInstances #-}

import Test.QuickCheck
import Data.Monoid
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Render.HtmlText
import System.IO (putStrLn)

-- Must implement an Eq instance for each Html instance we test
instance Eq HtmlText where
    a == b = renderHtmlText a == renderHtmlText b

newtype SpecialChar = SC { unSC :: Char } deriving Show
instance Arbitrary SpecialChar where
   arbitrary = do 
      x <- oneof [choose ('\0','\55295'), choose ('\57344','\1114111')]
      return (SC x)

instance Arbitrary Text where
    arbitrary     = T.pack `fmap` arbitrary

-- Tests for equalities detailed in Html typeclass

prop_RenderEmpty :: (Html h, Eq h) => h -> Bool
prop_RenderEmpty h = renderUnescapedText mempty == h

-- Distributivity (?)
prop_RenderDistrib :: (Html h, Eq h) => h -> Text -> Text -> Bool
prop_RenderDistrib h t1 t2 = left == right
  where
    left = h `mappend` renderUnescapedText t1 `mappend` renderUnescapedText t2
    right = h `mappend` renderUnescapedText (t1 `mappend` t2)

prop_SetAttribUnescaped :: (Html h, Eq h) => h -> Attributes -> Text -> Bool
prop_SetAttribUnescaped h a t = left == right
  where
    left = h `mappend` setUnescapedAttributes a (renderUnescapedText t)
    right = h `mappend` (renderUnescapedText t)

prop_AddAttribUnescaped :: (Html h, Eq h) => h -> Attributes -> Text -> Bool
prop_AddAttribUnescaped h a t = left == right
  where
    left = h `mappend` addUnescapedAttributes a (renderUnescapedText t)
    right = h `mappend` (renderUnescapedText t)

-- The Html object passed in MUST have an outer tag, so attributes can be set
prop_SetAttribOverwrite :: (Html h, Eq h) => h -> Attributes -> Attributes -> Text -> Bool
prop_SetAttribOverwrite h a1 a2 t = left == right
  where
    left = setUnescapedAttributes a1 (setUnescapedAttributes a2 h)
    right = setUnescapedAttributes a2 h

prop_AddSetAttribOverwrite :: (Html h, Eq h) => h -> Attributes -> Attributes -> Text -> Bool
prop_AddSetAttribOverwrite h a1 a2 t = left == right
  where
    left = addUnescapedAttributes a1 (setUnescapedAttributes a2 h)
    right = setUnescapedAttributes a2 h

orangeAttribs = [(T.pack("orange"), T.pack("mandarin"))]
appleAttribs = [(T.pack("apple"), T.pack("bramley"))]

pHtmlText :: HtmlText
pHtmlText = renderLeafElement $ T.pack "p"

runTests = do
    quickCheck $ prop_RenderEmpty (mempty :: HtmlText)
    quickCheck $ prop_RenderDistrib (mempty :: HtmlText)
    quickCheck $ prop_SetAttribUnescaped (mempty :: HtmlText) orangeAttribs
    quickCheck $ prop_AddAttribUnescaped (mempty :: HtmlText) orangeAttribs
    quickCheck $ prop_SetAttribOverwrite pHtmlText orangeAttribs appleAttribs
    quickCheck $ prop_AddSetAttribOverwrite pHtmlText orangeAttribs appleAttribs

printTests = do
    putStr "p orange=mandarin\t\t"
    putStrLn $ T.unpack $ renderHtmlText $ setUnescapedAttributes orangeAttribs pHtmlText
    putStrLn $ T.unpack $ renderHtmlText $ setUnescapedAttributes appleAttribs (setUnescapedAttributes orangeAttribs pHtmlText)
