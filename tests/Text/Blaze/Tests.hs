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
tests = []

-- Arbitrary instance for the HTML type.
--
instance Arbitrary (Html a) where
    arbitrary = arbitraryHtml 5

-- | Auxiliary function for the arbitrary instance of the HTML type, used
-- to limit the depth and size of the type.
--
arbitraryHtml :: Int           -- ^ Maximum depth.
              -> Gen (Html a)  -- ^ Resulting arbitrary HTML snippet.
arbitraryHtml depth = do 
    -- Choose the size (width) of this element.
    size <- choose (0, 10)

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
        size <- choose (0, 3)
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
