{-# LANGUAGE TypeSynonymInstances #-}

import Test.QuickCheck
import Data.Monoid
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Render.HtmlText

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

prop_NoAttribUnescaped :: (Html h, Eq h) => h -> Attributes -> Text -> Bool
prop_NoAttribUnescaped h a t = left == right
  where
    left = h `mappend` setUnescapedAttributes a (renderUnescapedText t)
    right = h `mappend` (renderUnescapedText t)

runTests = do
    quickCheck $ prop_RenderEmpty (mempty :: HtmlText)
    quickCheck $ prop_RenderDistrib (mempty :: HtmlText)
