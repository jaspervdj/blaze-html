-- | Example with BlazeHtml running behind the Snap Haskell web framework.
-- For more information on snap, you can refer to http://snapframework.com.
--
{-# LANGUAGE OverloadedStrings #-}
module SnapFramework where

import System (getArgs)

import Snap.Http.Server
import Snap.Types

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Renderer.Utf8 (renderHtml)

-- | A welcome page.
--
welcomePage :: Html
welcomePage = html $ do
    H.head $ do
        title $ "Snap & BlazeHtml"
    body $ do
        h1 $ "Snap & BlazeHtml"
        p $ "This is an example of BlazeHtml running behind the snap framework."

-- | Auxiliary function to render a BlazeHtml template to a @Snap ()@ type.
--
blazeTemplate :: Html -> Snap ()
blazeTemplate template = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml template

-- | Always return the welcome page.
--
site :: Snap ()
site = blazeTemplate welcomePage

-- | Snap main function.
--
main :: IO ()
main = do
    args <- getArgs
    let port = case args of
                   []  -> 8000
                   p:_ -> read p
    httpServe "*" port "myserver"
        (Just "access.log")
        (Just "error.log")
        site
