-- | A module containing a number of simple html examples. These examples are
-- unit tests, and all consist of some html combinators and the expected output.
{-# LANGUAGE OverloadedStrings #-}
module SimpleHtmlExamples
    ( simpleHtmlExamples
    ) where

import GHC.Exts (IsString (..))

import Test.HUnit ((@?=))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Text.BlazeHtml.Render.HtmlText
import Text.BlazeHtml.Html
import Text.BlazeHtml.Html as H
import Text.BlazeHtml.Attributes as A

-- | Group containing all the tests in this module.
simpleHtmlExamples = testGroup "SimpleHtmlExamples"
    [ testCase "simpleHtmlExample1" simpleHtmlExample1
    , testCase "simpleHtmlExample2" simpleHtmlExample2
    ]

-- | A first simple example.
simpleHtmlExample1 = htmlText html @?= fromString expected
  where
    html = runHtmlMonad $ do
        h1 "BlazeHtml"
        H.div $ do
            "This is a first sentence."
            <-> "This is a second sentence."
            <-> "Should be spaces in between."

    expected =  "<h1>BlazeHtml</h1><div>This is a first sentence. "
             ++ "This is a second sentence. Should be spaces in between.</div>"

-- | Test some attribute setting.
simpleHtmlExample2 = htmlText html @?= fromString expected
  where
    html = runHtmlMonad $ do
        h1 ! A.class_ "fancy" $ "Title here."
        p ! A.id "first-paragraph" $ do
            "No attributes."
            <-> (em ! [A.id "nested", width "4"] $ "And nested thingies.")
        H.div ! [A.id "div-foo", A.class_ "small"] $ do
            "Hello there!"
            <-> "Nesting and multiple attributes."

    expected =  "<h1 class=\"fancy\">Title here.</h1>"
             ++ "<p id=\"first-paragraph\">"
             ++ "No attributes. "
             ++ "<em id=\"nested\" width=\"4\">And nested thingies.</em></p>"
             ++ "<div id=\"div-foo\" class=\"small\">Hello there! "
             ++ "Nesting and multiple attributes.</div>"
