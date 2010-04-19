-- | This is a possible library implementation experiment and benchmark.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div)

import Criterion.Main
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import GHC.Exts (IsString, fromString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Text.Blaze.Internal.Utf8Builder

main = defaultMain
    [ bench "bigTable" $ nf (BL.length . bigTable) myTable
    , bench "basic" $ nf (BL.length . basic) myData
    ]
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}

    myData :: (Text, Text, [Text])
    myData = ("Just a test", "joe", items)
    {-# NOINLINE myData #-}

    items :: [Text]
    items = map (("Number " `mappend`) . T.pack . show) [1 .. 14]
    {-# NOINLINE items #-}

newtype Html a = Html { runHtml :: Builder -> Builder }

text :: Text -> Html a
text = Html . const . fromHtmlText
{-# INLINE text #-}

rawByteString :: ByteString -> Html a
rawByteString = Html . const . fromRawByteString
{-# INLINE rawByteString #-}

showHtml :: Show a => a -> Html a
showHtml = Html . const . fromHtmlString . show
{-# INLINE showHtml #-}


tag :: ByteString -> Html a -> Html a
tag name inner = Html $ \attrs ->
    fromRawAscii7Char '<' `mappend` (fromRawByteString name
                          `mappend` (attrs
                          `mappend` (fromRawAscii7Char '>'
                          `mappend` (runHtml inner mempty
                          `mappend` (fromRawByteString "</" 
                          `mappend` (fromRawByteString name
                          `mappend` (fromRawAscii7Char '>')))))))
{-# INLINE tag #-}

addAttr :: ByteString -> Text -> Html a -> Html a
addAttr key value h = Html $ \attrs ->
    runHtml h $ attrs `mappend` (fromRawAscii7Char ' '
                      `mappend` (fromRawByteString key
                      `mappend` (fromRawByteString "=\""
                      `mappend` (fromHtmlText value
                      `mappend` (fromRawAscii7Char '"')))))
{-# INLINE addAttr #-}

tag' :: ByteString -> ByteString -> Html a -> Html a
tag' begin end = \inner -> Html $ \attrs ->
    fromRawByteString begin
      `mappend` attrs
      `mappend` fromRawAscii7Char '>'
      `mappend` runHtml inner mempty
      `mappend` fromRawByteString end
{-# INLINE tag' #-}

table :: Html a -> Html a
table = let tableB, tableE :: ByteString
            tableB = "<table"
            tableE = "</table>"
            {-# NOINLINE tableB #-}
            {-# NOINLINE tableE #-}
        in tag' tableB tableE
{-# INLINE table #-}

tr :: Html a -> Html a
tr = let trB, trE :: ByteString
         trB = "<tr"
         trE = "</tr>"
         {-# NOINLINE trB #-}
         {-# NOINLINE trE #-}
     in tag' trB trE
{-# INLINE tr #-}

td :: Html a -> Html a
td = let tdB, tdE :: ByteString
         tdB = "<td"
         tdE = "</td>"
         {-# NOINLINE tdB #-}
         {-# NOINLINE tdE #-}
     in tag' tdB tdE
{-# INLINE td #-}

renderHtml :: Html a -> BL.ByteString
renderHtml h = toLazyByteString $ runHtml h mempty

instance Monoid (Html a) where
    mempty = Html $ \_ -> mempty
    {-# INLINE mempty #-}
    (Html h1) `mappend` (Html h2) = Html $ \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = Html $ \attrs ->
        foldr (\h k -> runHtml h attrs `mappend` k) mempty hs
    {-# INLINE mconcat #-}

instance Monad Html where
    return a = mempty
    {-# INLINE return #-}
    (Html h1) >> (Html h2) = Html $
        \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (error "_|_")
    {-# INLINE (>>=) #-}

bigTable :: [[Int]] -> BL.ByteString
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . showHtml) r

html :: Html a -> Html a
html inner = 
  -- a too long string for the fairness of comparison
  rawByteString "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tag "html" inner
  
header = tag' "<header" "</header>"
title  = tag' "<title"  "</title>"
body   = tag' "<body"   "</body>"
div    = tag' "<div"    "</div>"
h1     = tag' "<h1"     "</h1>"
h2     = tag' "<h2"     "</h2>"
li     = tag' "<li"     "</li>"

-- THE following seems to be the desired recipe: sharing of data, inlining of
-- control.
pB = "<p"
pE = "</p>"
p  = tag' pB pE
{-# NOINLINE pB #-}
{-# NOINLINE pE #-}
{-# INLINE p #-}

idA = addAttr "id"

hello1, hello2, hello3, loop :: ByteString
hello1 = "Hello, "
hello2 = "Hello, me!"
hello3 = "Hello, world!"
loop   = "Loop"

static = Html . const . fromRawByteString
{-# INLINE static #-}

basic :: (Text, Text, [Text]) -- ^ (Title, User, Items)
      -> BL.ByteString
basic (title', user, items) = renderHtml $ html $ mconcat
    [ header $ title $ text title'
    , body $ mconcat
        [ div $ idA "header" (h1 $ text title')
        , p $ static hello1 `mappend` text user `mappend` text "!"
        , p $ static hello2
        , p $ static hello3
        , h2 $ static loop
        , mconcat $ map (li . text) items
        , idA "footer" (div $ mempty)
        ]
    ]
