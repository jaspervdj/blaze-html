-- | Main module to run all tests.
--
module TestSuite where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Text.Blaze.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.Blaze.Tests" Text.Blaze.Tests.tests
    ]
