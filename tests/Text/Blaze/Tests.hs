{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div, id)
import Data.Monoid (mconcat, mempty, mappend)
import Control.Monad (replicateM, forM_, sequence_)
import Control.Applicative ((<$>))
import Data.Word (Word8)
import Data.Char (ord)
import Data.List (isInfixOf)

import Data.Text.Lazy.Encoding (encodeUtf8)
import Debug.Trace (traceShow)
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Lazy as LB
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Renderer.Utf8 as Utf8 (renderHtml)
import qualified Text.Blaze.Renderer.Text as Text (renderHtml)
import qualified Text.Blaze.Renderer.String as String (renderHtml)
import Blaze.ByteString.Builder as B (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 as B (fromString)

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
        , testCase     "template case 3"           template3
        , testCase     "template case 4"           template4
        , testCase     "template case 5"           template5
        , testCase     "template case 6"           template6
        , testCase     "template case 7"           template7
        , testCase     "template case 8"           template8
        , testProperty "valid UTF-8"               isValidUtf8
        , testProperty "external </ sequence"      externalEndSequence
        , testProperty "well nested <>"            wellNestedBrackets
        ]

-- | Render HTML to an UTF-8 encoded ByteString using the String renderer
--
renderUsingString :: Html -> LB.ByteString
renderUsingString = toLazyByteString . fromString . String.renderHtml

-- | Render HTML to an UTF-8 encoded ByteString using the Text renderer
--
renderUsingText :: Html -> LB.ByteString
renderUsingText = encodeUtf8 . Text.renderHtml

-- | Render HTML to an UTF-8 encoded ByteString using the Utf8 renderer
--
renderUsingUtf8 :: Html -> LB.ByteString
renderUsingUtf8 = Utf8.renderHtml

-- | Auxiliary function to create a template test
--
testTemplate :: LB.ByteString  -- ^ Expected output
             -> Html           -- ^ HTML to render
             -> Assertion      -- ^ Resulting text
testTemplate expected html = condition @?
    "Expected: " ++ show expected ++ " but got: " ++ show (r1, r2, r3)
  where
    r1 = renderUsingString html
    r2 = renderUsingText html
    r3 = renderUsingUtf8 html
    condition =  expected == r1 && expected == r2 && expected == r3

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
escaping1 = testTemplate "&quot;&amp;&quot;" (string "\"&\"")

-- | Simple escaping test case.
--
escaping2 :: Assertion
escaping2 = testTemplate "&lt;img&gt;" (text "<img>")

-- | Escaped content cannot contain certain characters.
--
postEscapingCharacters :: String -> Bool
postEscapingCharacters str =
    LB.all (`notElem` forbidden) $ Utf8.renderHtml (string str)
  where
    forbidden = map (fromIntegral . ord) "\"'<>"

-- | Simple template test case
--
template1 :: Assertion
template1 = testTemplate expected template
  where
    expected = "<div id=\"foo\"><p>banana</p><span>banana</span></div>"
    template = div ! id "foo" $ do
        p "banana"
        H.span "banana"

-- | Simple template test case
--
template2 :: Assertion
template2 = testTemplate expected template
  where
    expected = "<img src=\"foo.png\" alt=\"bar\" />"
    template = img ! src "foo.png" ! alt "bar"

-- | Simple template test case
--
template3 :: Assertion
template3 = testTemplate expected template
  where
    -- Note how we write λ in UTF-8 encoded notation
    expected = "<span id=\"&amp;\">\206\187</span>"
    template = H.span ! id "&" $ "λ"

-- | Simple template test case
--
template4 :: Assertion
template4 = testTemplate expected template
  where
    -- Three-byte UTF-8
    expected = "\226\136\128x. x \226\136\136 A"
    template = "∀x. x ∈ A"

-- | Simple template test case
--
template5 :: Assertion
template5 = testTemplate expected template
  where
    expected = "<li>4</li><li>5</li><li>6</li>"
    template = forM_ [4 .. 6] (li . showHtml)

-- | Simple template test case
--
template6 :: Assertion
template6 = testTemplate expected template
  where
    expected = "<br /><img /><area />"
    template = sequence_ [br, img, area]

-- | Simple template test case
--
template7 :: Assertion
template7 = testTemplate expected template
  where
    expected = "$6, \226\130\172\&7.01, \194\163\&75"
    template = "$6, €7.01, £75"

-- | Simple template test case
--
template8 :: Assertion
template8 = testTemplate expected template
  where
    expected = "<p data-foo=\"bar\">A paragraph</p>"
    template = p ! (dataAttribute "foo" "bar") $ "A paragraph"

-- | Check if the produced bytes are valid UTF-8
--
isValidUtf8 :: Html -> Bool
isValidUtf8 = isValidUtf8' . LB.unpack . Utf8.renderHtml
  where
    isIn x y z = (x <= z) && (z <= y)
    isValidUtf8' :: [Word8] -> Bool
    isValidUtf8' [] = True
    isValidUtf8' (x:t)
        -- One byte
        | isIn 0x00 0x7f x = isValidUtf8' t
        -- Two bytes
        | isIn 0xc0 0xdf x = case t of
            (y:t) -> isIn 0x80 0xbf y && isValidUtf8' t
            _     -> False
        -- Three bytes
        | isIn 0xe0 0xef x = case t of
            (y:z:t) -> all (isIn 0x80 0xbf) [y, z] && isValidUtf8' t
            _       -> False
        -- Four bytes
        | isIn 0xf0 0xf7 x = case t of
            (y:z:u:t) -> all (isIn 0x80 0xbf) [y, z, u] && isValidUtf8' t 
            _         -> False
        | otherwise = False

-- | Check if the "</" sequence does not appear in @<script>@ or @<style>@ tags.
--
externalEndSequence :: String -> Bool
externalEndSequence = not . isInfixOf "</" . LBC.unpack
                    . Utf8.renderHtml . external . string

-- | Check that the "<>" characters are well-nested.
--
wellNestedBrackets :: Html -> Bool
wellNestedBrackets = wellNested False . LBC.unpack . Utf8.renderHtml
  where
    wellNested isOpen [] = not isOpen
    wellNested isOpen (x:xs) = case x of
        '<' -> if isOpen then False else wellNested True xs
        '>' -> if isOpen then wellNested False xs else False
        _   -> wellNested isOpen xs

-- Show instance for the HTML type, so we can debug.
--
instance Show Html where
    show = String.renderHtml

-- Eq instance for the HTML type, so we can compare the results.
--
instance Eq Html where
    h1 == h2 =  renderUsingString h1 == renderUsingString h2
             && renderUsingText h1   == renderUsingText h2
             && renderUsingUtf8 h1   == renderUsingUtf8 h2
             -- Some cross-checks
             && renderUsingString h1 == renderUsingText h2
             && renderUsingText h1   == renderUsingUtf8 h2

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
