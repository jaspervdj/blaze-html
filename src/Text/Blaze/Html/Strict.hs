{-# LANGUAGE OverloadedStrings #-}
-- | Strict HTML 4.01 combinators.
module Text.Blaze.Html.Strict
    ( table
    , tr
    , td
    , html
    , header
    , title
    , body
    , div
    , h1
    , h2
    , li
    , p
    ) where

import Data.Monoid (mappend)
import Prelude hiding (div)

import Data.ByteString.Char8 (ByteString)

import Text.Blaze

table :: Html -> Html
table = let tableB, tableE :: ByteString
            tableB = "<table"
            tableE = "</table>"
            {-# NOINLINE tableB #-}
            {-# NOINLINE tableE #-}
        in tag tableB tableE
{-# INLINE table #-}

tr :: Html -> Html
tr = let trB, trE :: ByteString
         trB = "<tr"
         trE = "</tr>"
         {-# NOINLINE trB #-}
         {-# NOINLINE trE #-}
     in tag trB trE
{-# INLINE tr #-}

td :: Html -> Html
td = let tdB, tdE :: ByteString
         tdB = "<td"
         tdE = "</td>"
         {-# NOINLINE tdB #-}
         {-# NOINLINE tdE #-}
     in tag tdB tdE
{-# INLINE td #-}

html :: Html -> Html
html inner = 
  -- a too long string for the fairness of comparison
  rawByteString "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tag "<html" "</html>" inner
  
header = tag "<header" "</header>"
title  = tag "<title"  "</title>"
body   = tag "<body"   "</body>"
div    = tag "<div"    "</div>"
h1     = tag "<h1"     "</h1>"
h2     = tag "<h2"     "</h2>"
li     = tag "<li"     "</li>"

-- THE following seems to be the desired recipe: sharing of data, inlining of
-- control.
pB = "<p"
pE = "</p>"
p  = tag pB pE
{-# NOINLINE pB #-}
{-# NOINLINE pE #-}
{-# INLINE p #-}
