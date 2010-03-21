module Criterion where

import Criterion.Main (defaultMain)
import Criterion.Benchmarks

-- | No configuration, straight run sockets followed by file tests.
main :: IO ()
main = defaultMain benchmarks
