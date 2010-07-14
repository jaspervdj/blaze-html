#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Process (rawSystem)

import Util.GenerateHtmlCombinators (generateHtmlCombinators)

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks { preConf = preConf' }

    -- Simple hook that generates the code for the HTML combinators.
    preConf' _ _ = do
        generateHtmlCombinators
        return emptyHookedBuildInfo
