-- | A whole bunch of simple test cases
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html.Tests
    ( tests
    ) where

import Prelude hiding (div, id)
import Control.Monad (forM_)
import Data.Monoid (mempty, mappend, mconcat)

import Data.Text (Text)
import Test.HUnit ((@=?))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test)
import qualified Data.ByteString.Lazy.Char8 as LBC

import Text.Blaze.Html.Tests.Util
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H

-- | Type for a simple HTML test. This data type contains the expected output
-- and the HTML template.
--
data HtmlTest = HtmlTest LBC.ByteString Html

-- | Create tests from an HTML test
--
makeTests :: String -> HtmlTest -> [Test]
makeTests baseName (HtmlTest expected h) =
    [ testCase (baseName ++ " (String)") $ expected @=? renderUsingString h
    , testCase (baseName ++ " (Text)") $ expected @=? renderUsingText h
    , testCase (baseName ++ " (Utf8)") $ expected @=? renderUsingUtf8 h
    ]

-- | Actual tests
--
tests :: [Test]
tests = concatMap (uncurry makeTests) $ zip names
    -- Simple cases
    [ HtmlTest "<div id=\"foo\"><p>banana</p><span>banana</span></div>" $
        div ! id "foo" $ do
            p "banana"
            H.span "banana"

    , HtmlTest "<img src=\"foo.png\" alt=\"bar\">" $
        img ! src "foo.png" ! alt "bar"

    -- Escaping cases
    , HtmlTest "&#34;&amp;&#34;" "\"&\""

    , HtmlTest "&lt;img&gt;" $ toHtml ("<img>" :: Text)

    , HtmlTest "&#34;'&#34;" "\"'\""

    , HtmlTest "<img src=\"&amp;\">" $ img ! src "&"

    -- Pre-escaping cases
    , HtmlTest "<3 Haskell" $ preEscapedToMarkup ("<3 Haskell" :: String)

    , HtmlTest "<script />" $ preEscapedToMarkup ("<script />" :: Text)

    , HtmlTest "<p class=\"'&!;\">bad</p>" $
        p ! class_ (preEscapedToValue ("'&!;" :: String)) $ "bad"

    -- Unicode cases
    , HtmlTest "<span id=\"&amp;\">\206\187</span>" $
        H.span ! id "&" $ "λ"

    , HtmlTest "\226\136\128x. x \226\136\136 A"
        "∀x. x ∈ A"

    , HtmlTest "$6, \226\130\172\&7.01, \194\163\&75"
        "$6, €7.01, £75"

    -- Control cases
    , HtmlTest "<li>4</li><li>5</li><li>6</li>" $
        forM_ [4 :: Int .. 6] (li . toHtml)

    , HtmlTest "<br><img><area>" $
        sequence_ [br, img, area]

    -- Attribute tests
    , HtmlTest "<p data-foo=\"bar\">A paragraph</p>" $
        p ! (dataAttribute "foo" "bar") $ "A paragraph"

    , HtmlTest "<p>Hello</p>" $ p ! mempty $ "Hello"

    , HtmlTest "<img src=\"foo.png\" alt=\"foo\">" $
        img ! (src "foo.png" `mappend` alt "foo")

    -- ToHtml/ToValue tests
    , HtmlTest "12345678910" $ mconcat $ map toHtml [1 :: Int .. 10]

    , HtmlTest "<img src=\"funny-picture-4.png\">" $
        img ! src ("funny-picture-" `mappend` toValue (4 :: Integer)
                                    `mappend` ".png")

    , HtmlTest "abcdefghijklmnopqrstuvwxyz" $ forM_ ['a' .. 'z'] toHtml

    -- Custom elements/attributes tests
    , HtmlTest "<p>A paragraph</p>" $
        customParent "p" $ "A paragraph"

    , HtmlTest "<img src=\"foo.png\">" $
        customLeaf "img" False ! src "foo.png"

    , HtmlTest "<img />" $ customLeaf "img" True

    , HtmlTest "<p dojoType=\"select\">A paragraph</p>" $
        p ! (customAttribute "dojoType" "select") $ "A paragraph"
    ]
  where
    names = map (("Test case " ++) . show) [1 :: Int ..]
