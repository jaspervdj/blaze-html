{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div, id)
import Data.Monoid (mconcat, mempty, mappend)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes

tests :: [Test]
tests = [ testProperty "left identity Monoid law"  monoidLeftIdentity
        , testProperty "right identity Monoid law" monoidRightIdentity
        , testProperty "associativity Monoid law"  monoidAssociativity
        , testProperty "mconcat Monoid law"        monoidConcat
        ]

-- | The left identity Monoid law.
--
monoidLeftIdentity :: Html a -> Bool
monoidLeftIdentity h = mappend mempty h == h

-- | The right identity Monoid law.
--
monoidRightIdentity :: Html a -> Bool
monoidRightIdentity h = mappend h mempty == h

-- | The associativity Monoid law.
--
monoidAssociativity :: Html a -> Html a -> Html a -> Bool
monoidAssociativity x y z = mappend x (mappend y z) == mappend (mappend x y) z

-- | Concatenation Monoid law.
--
monoidConcat :: [Html a] -> Bool
monoidConcat xs = mconcat xs == foldr mappend mempty xs

-- Show instance for the HTML type, so we can debug.
--
instance Show (Html a) where
    show = show . renderHtml

-- Eq instance for the HTML type, so we can compare the results.
--
instance Eq (Html a) where
    h1 == h2 = renderHtml h1 == renderHtml h2

-- Arbitrary instance for the HTML type.
--
instance Arbitrary (Html a) where
    arbitrary = arbitraryHtml 4

-- | Auxiliary function for the arbitrary instance of the HTML type, used
-- to limit the depth and size of the type.
--
arbitraryHtml :: Int           -- ^ Maximum depth.
              -> Gen (Html a)  -- ^ Resulting arbitrary HTML snippet.
arbitraryHtml depth = do 
    -- Choose the size (width) of this element.
    size <- choose (0, 3)

    -- Generate `size` new HTML snippets.
    children <- replicateM size arbitraryChild

    -- Return a concatenation of these children.
    return $ mconcat children
  where
    -- Generate an arbitrary child. Do not take a parent when we have no depth
    -- left, obviously.
    arbitraryChild = do
        child <- oneof $  [arbitraryLeaf, arbitraryString]
                       ++ [arbitraryParent | depth > 0]

        -- Generate some attributes for the child.
        size <- choose (0, 4)
        attributes <- replicateM size arbitraryAttribute
        return $ foldl (!) child attributes

    -- Generate an arbitrary parent element.
    arbitraryParent = do
        parent <- elements [p, div, table]
        parent <$> arbitraryHtml (depth - 1)

    -- Generate an arbitrary leaf element.
    arbitraryLeaf = oneof $ map return [img, br, area]

    -- Generate arbitrary string element.
    arbitraryString = do
        s <- arbitrary
        return $ string s

    -- Generate an arbitrary HTML attribute.
    arbitraryAttribute = do
        attribute <- elements [id, class_, name]
        value <- arbitrary
        return $ attribute $ stringValue value
