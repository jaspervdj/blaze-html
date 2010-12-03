{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div, id)
import Data.Monoid (mempty)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Word (Word8)
import Data.Char (ord)
import Data.List (isInfixOf)

import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Lazy as LB
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes (id, class_, name)
import Text.Blaze.Internal
import Text.Blaze.Tests.Util

tests :: [Test]
tests = [ testProperty "left identity Monoid law"  monoidLeftIdentity
        , testProperty "right identity Monoid law" monoidRightIdentity
        , testProperty "associativity Monoid law"  monoidAssociativity
        , testProperty "mconcat Monoid law"        monoidConcat
        , testProperty "post escaping characters"  postEscapingCharacters
        , testProperty "valid UTF-8"               isValidUtf8
        , testProperty "external </ sequence"      externalEndSequence
        , testProperty "well nested <>"            wellNestedBrackets
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

-- | Escaped content cannot contain certain characters.
--
postEscapingCharacters :: String -> Bool
postEscapingCharacters str =
    LB.all (`notElem` forbidden) $ renderUsingUtf8 (string str)
  where
    forbidden = map (fromIntegral . ord) "\"'<>"

-- | Check if the produced bytes are valid UTF-8
--
isValidUtf8 :: Html -> Bool
isValidUtf8 = isValidUtf8' . LB.unpack . renderUsingUtf8
  where
    isIn x y z = (x <= z) && (z <= y)
    isValidUtf8' :: [Word8] -> Bool
    isValidUtf8' [] = True
    isValidUtf8' (x:t)
        -- One byte
        | isIn 0x00 0x7f x = isValidUtf8' t
        -- Two bytes
        | isIn 0xc0 0xdf x = case t of
            (y:t') -> isIn 0x80 0xbf y && isValidUtf8' t'
            _      -> False
        -- Three bytes
        | isIn 0xe0 0xef x = case t of
            (y:z:t') -> all (isIn 0x80 0xbf) [y, z] && isValidUtf8' t'
            _        -> False
        -- Four bytes
        | isIn 0xf0 0xf7 x = case t of
            (y:z:u:t') -> all (isIn 0x80 0xbf) [y, z, u] && isValidUtf8' t'
            _          -> False
        | otherwise = False

-- | Check if the "</" sequence does not appear in @<script>@ or @<style>@ tags.
--
externalEndSequence :: String -> Bool
externalEndSequence = not . isInfixOf "</" . LBC.unpack
                    . renderUsingUtf8 . external . string

-- | Check that the "<>" characters are well-nested.
--
wellNestedBrackets :: Html -> Bool
wellNestedBrackets = wellNested False . LBC.unpack . renderUsingUtf8
  where
    wellNested isOpen [] = not isOpen
    wellNested isOpen (x:xs) = case x of
        '<' -> if isOpen then False else wellNested True xs
        '>' -> if isOpen then wellNested False xs else False
        _   -> wellNested isOpen xs

-- Show instance for the HTML type, so we can debug.
--
instance Show Html where
    show = show . renderUsingUtf8

-- Eq instance for the HTML type, so we can compare the results.
--
instance Eq Html where
    x == y =  renderUsingString x == renderUsingString y
           && renderUsingText x   == renderUsingText y
           && renderUsingUtf8 x   == renderUsingUtf8 y
           -- Some cross-checks
           && renderUsingString x == renderUsingText y
           && renderUsingText x   == renderUsingUtf8 y

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
        attr <- elements [id, class_, name]
        value <- arbitrary
        return $ attr $ stringValue value
