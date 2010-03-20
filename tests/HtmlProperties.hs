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

instance Arbitrary Char where
    arbitrary     = oneof [choose ('\0','\55295'), choose ('\57344','\1114111')]
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary Text where
    arbitrary     = T.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (T.unpack s)

-- Tests for equalities detailed in Html typeclass

prop_RenderEmpty :: (Html h, Eq h) => h -> Bool
prop_RenderEmpty h = renderUnescapedText mempty == h

-- Distributivity (?)
prop_RenderDistrib :: (Html h, Eq h) => h -> Text -> Text -> Bool
prop_RenderDistrib h t1 t2 = left == right
  where
    left = h `mappend` renderUnescapedText t1 `mappend` renderUnescapedText t2
    right = renderUnescapedText (t1 `mappend` t2)

prop_ModAttrDistrib :: (Html h, Eq h) => 
    h -> (AttributeManipulation -> AttributeManipulation) -> Text -> Text -> Bool
prop_ModAttrDistrib h f t1 t2 = left == right
  where
    
--    modifyUnescapedAttributes f (t1 `mappend` t2) = 
--    modifyUnescapedAttributes t1 `mappend` modifyUnescapedAttributes t2
