module Util.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Util.Sanitize (sanitize)

tests :: [Test]
tests = [ testCase "sanitize case 1" sanitize1
        , testCase "sanitize case 2" sanitize2
        ]

-- | Simple sanitize test case
--
sanitize1 :: Assertion
sanitize1 = "class_" @=? sanitize "CLASS"

-- | Simple sanitize test case
--
sanitize2 :: Assertion
sanitize2 = "httpEquiv" @=? sanitize "http-equiv"
