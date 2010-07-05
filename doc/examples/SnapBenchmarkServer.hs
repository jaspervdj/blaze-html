{-# LANGUAGE OverloadedStrings #-}
module SnapBenchmarkServer where

import System (getArgs)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Control.Applicative ((<$>))
import qualified Data.Map as M

import Snap.Http.Server
import Snap.Types

import qualified Data.ByteString.Char8 as SBC

import Text.Blaze.Renderer.Utf8 (renderHtml)
import BenchmarkServer hiding (main)
import Utf8Html hiding (main)

rootHandler :: Snap ()
rootHandler = writeLBS $ renderHtml root

benchmarkHandler :: Snap ()
benchmarkHandler = do
     b <- fromMaybe "" <$> getParam "b"
     case M.lookup (map toLower $ SBC.unpack b) benchmarkMap of
         Just (HtmlBenchmark _ f x _) -> writeLBS $ renderHtml $ f x
         Nothing                      -> notFound

notFound :: Snap ()
notFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    writeBS "404 error"
    r <- getResponse
    finishWith r

site :: Snap ()
site = route [ ("",    rootHandler)
             , ("/:b", benchmarkHandler)
             ]

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                   []  -> 8000
                   p:_ -> read p
    httpServe "*" port "myserver" (Just "access.log") (Just "error.log") site
