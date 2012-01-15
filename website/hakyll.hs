{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((>>>))

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
        compile $
            pageCompiler >>>
            applyTemplateCompiler "templates/default.html" >>>
            relativizeUrlsCompiler

    match "templates/*" $ compile templateCompiler
  where
    pages = list
        [ "index.markdown"
        , "tutorial.lhs"
        , "about.markdown"
        , "benchmarks.markdown"
        ]


config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be/blaze"
    }
