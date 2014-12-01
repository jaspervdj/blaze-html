{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "docs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match pages $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
  where
    pages = fromList
        [ "index.markdown"
        , "tutorial.lhs"
        , "about.markdown"
        , "benchmarks.markdown"
        ]


config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be/blaze"
    }
