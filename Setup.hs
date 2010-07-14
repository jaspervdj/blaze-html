#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Process (rawSystem)

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks { preConf = generateHtmlCombinators }

generateHtmlCombinators :: Args -> ConfigFlags -> IO HookedBuildInfo
generateHtmlCombinators _ _ = do
    -- Portability needs to be tested.
    _ <- rawSystem "runghc" ["-iutil", "util/GenerateHtmlVariant.hs"]
    return emptyHookedBuildInfo
