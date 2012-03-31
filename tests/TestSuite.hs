-- | Main module to run all tests.
--
module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Blaze.Tests.Cases
import qualified Util.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.Blaze.Tests.Cases" Text.Blaze.Tests.Cases.tests
    , testGroup "Util.Tests"             Util.Tests.tests
    ]
