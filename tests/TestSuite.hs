-- | Main module to run all tests.
--
module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Blaze.Html.Tests
import qualified Util.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.Blaze.Html.Tests" Text.Blaze.Html.Tests.tests
    , testGroup "Util.Tests"             Util.Tests.tests
    ]
