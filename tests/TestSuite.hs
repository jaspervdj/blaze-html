-- | Main module to run all tests.
--
module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Blaze.Tests
import qualified Text.Blaze.Tests.Cases
import qualified Util.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.Blaze.Tests"       Text.Blaze.Tests.tests
    , testGroup "Text.Blaze.Tests.Cases" Text.Blaze.Tests.Cases.tests
    , testGroup "Util.Tests"             Util.Tests.tests
    ]
