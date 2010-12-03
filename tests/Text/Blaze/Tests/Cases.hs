-- | A whole bunch of simple test cases
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Tests.Cases
    ( tests
    ) where

import Prelude hiding (div, id)
import Control.Monad (forM_)

import Test.HUnit ((@=?))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test)
import qualified Data.ByteString.Lazy.Char8 as LBC

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Tests.Util

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

    , HtmlTest "<img src=\"foo.png\" alt=\"bar\" />" $
        img ! src "foo.png" ! alt "bar"

    -- Escaping cases
    , HtmlTest "&quot;&amp;&quot;" "\"&\""

    , HtmlTest "&lt;img&gt;" $ text "<img>"

    , HtmlTest "&quot;&#39;&quot;" "\"'\""

    , HtmlTest "<img src=\"&amp;\" />" $ img ! src "&"

    -- Pre-escaping cases
    , HtmlTest "<3 Haskell" $ preEscapedText "<3 Haskell"

    , HtmlTest "<script />" $ preEscapedString "<script />"

    , HtmlTest "<p class=\"'&!;\">bad</p>" $
        p ! class_ (preEscapedTextValue "'&!;") $ "bad"

    -- Unicode cases
    , HtmlTest "<span id=\"&amp;\">\206\187</span>" $
        H.span ! id "&" $ "λ"

    , HtmlTest "\226\136\128x. x \226\136\136 A"
        "∀x. x ∈ A"

    , HtmlTest "$6, \226\130\172\&7.01, \194\163\&75"
        "$6, €7.01, £75"

    -- Control cases
    , HtmlTest "<li>4</li><li>5</li><li>6</li>" $
        forM_ [4 :: Int .. 6] (li . showHtml)

    , HtmlTest "<br /><img /><area />" $
        sequence_ [br, img, area]

    -- Attribute tests
    , HtmlTest "<p data-foo=\"bar\">A paragraph</p>" $
        p ! (dataAttribute "foo" "bar") $ "A paragraph"

    , HtmlTest "<p dojoType=\"select\">A paragraph</p>" $
        p ! (customAttribute "dojoType" "select") $ "A paragraph"
    ]
  where
    names = map (("Test case " ++) . show) [1 :: Int ..]
