{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div)
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
    size <- choose (0, 10 :: Int)

    -- Generate `size` new HTML snippets.
    children <- replicateM size arbitraryChild

    -- Return a concatenation of these children.
    return $ mconcat children
  where
    -- Generate an arbitrary child. Do not take a parent when we have no depth
    -- left, obviously.
    -- TODO: set attributes here?
    arbitraryChild = oneof $
        [arbitraryLeaf, arbitraryString] ++ [arbitraryParent | depth > 0]

    -- Generate an arbitrary parent element.
    arbitraryParent =
        oneof $ map (<$> arbitraryHtml (depth - 1)) [p, div, table]

    -- Generate an arbitrary leaf element.
    arbitraryLeaf = oneof $ map return [img, br, area]

    -- Generate arbitrary string element.
    arbitraryString = do
        s <- arbitrary
        return $ string s
