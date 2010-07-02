{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div, id)
import Data.Monoid (mconcat, mempty, mappend)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Word (Word8)
import Data.Char (ord)

import Debug.Trace (traceShow)
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Lazy as LB
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Utf8 (renderHtml)

tests :: [Test]
tests = [ testProperty "left identity Monoid law"  monoidLeftIdentity
        , testProperty "right identity Monoid law" monoidRightIdentity
        , testProperty "associativity Monoid law"  monoidAssociativity
        , testProperty "mconcat Monoid law"        monoidConcat
        , testCase     "escaping case 1"           escaping1
        , testCase     "escaping case 2"           escaping2
        , testProperty "post escaping characters"  postEscapingCharacters
        , testCase     "template case 1"           template1
        , testCase     "template case 2"           template2
        ]

-- | The left identity Monoid law.
--
monoidLeftIdentity :: Html -> Bool
monoidLeftIdentity h = (return () >> h) == h

-- | The right identity Monoid law.
--
monoidRightIdentity :: Html -> Bool
monoidRightIdentity h = (h >> return ()) == h

-- | The associativity Monoid law.
--
monoidAssociativity :: Html -> Html -> Html -> Bool
monoidAssociativity x y z = (x >> (y >> z)) == ((x >> y) >> z)

-- | Concatenation Monoid law.
--
monoidConcat :: [Html] -> Bool
monoidConcat xs = sequence_ xs == foldr (>>) (return ()) xs

-- | Simple escaping test case.
--
escaping1 :: Assertion
escaping1 = "&quot;&amp;&quot;" @=? renderHtml (string "\"&\"")

-- | Simple escaping test case.
--
escaping2 :: Assertion
escaping2 = "&lt;img&gt;" @=? renderHtml (text "<img>")

-- | Escaped content cannot contain certain characters.
--
postEscapingCharacters :: String -> Bool
postEscapingCharacters str =
    LB.all (`notElem` forbidden) $ renderHtml (string str)
  where
    forbidden = map (fromIntegral . ord) "\"'<>"

-- | Simple template test case
--
template1 :: Assertion
template1 = expected @=? renderHtml template
  where
    expected = "<div id=\"foo\"><p>banana</p><span>banana</span></div>"
    template = div ! id "foo" $ do
        p "banana"
        H.span "banana"

-- | Simple template test case
--
template2 :: Assertion
template2 = expected @=? renderHtml template
  where
    expected = "<img src=\"foo.png\" alt=\"bar\" />"
    template = img ! src "foo.png" ! alt "bar"

-- Show instance for the HTML type, so we can debug.
--
instance Show Html where
    show = show . renderHtml

-- Eq instance for the HTML type, so we can compare the results.
--
instance Eq Html where
    h1 == h2 = renderHtml h1 == renderHtml h2

-- Arbitrary instance for the HTML type.
--
instance Arbitrary Html where
    arbitrary = arbitraryHtml 4

-- | Auxiliary function for the arbitrary instance of the HTML type, used
-- to limit the depth and size of the type.
--
arbitraryHtml :: Int       -- ^ Maximum depth.
              -> Gen Html  -- ^ Resulting arbitrary HTML snippet.
arbitraryHtml depth = do 
    -- Choose the size (width) of this element.
    size <- choose (0, 3)

    -- Generate `size` new HTML snippets.
    children <- replicateM size arbitraryChild

    -- Return a concatenation of these children.
    return $ sequence_ children
  where
    -- Generate an arbitrary child. Do not take a parent when we have no depth
    -- left, obviously.
    arbitraryChild = do
        child <- oneof $  [arbitraryLeaf, arbitraryString, return mempty]
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
