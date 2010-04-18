-- | This is a possible library implementation experiment and benchmark.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import Data.Monoid (Monoid, mempty, mconcat, mappend)

import Criterion.Main
import Data.ByteString.Char8 (ByteString)
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
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

newtype Html = Html { runHtml :: Builder -> Builder }

text :: Text -> Html
text = Html . const . fromHtmlText
{-# INLINE text #-}

rawByteString :: ByteString -> Html
rawByteString = Html . const . fromSmallByteString
{-# INLINE rawByteString #-}

showAscii7Html :: Show a => a -> Html
showAscii7Html = Html . const . fromAscii7Show
{-# INLINE showAscii7Html #-}

tag :: ByteString -> Html -> Html
tag name inner = Html $ \attrs ->
    fromAscii7Char '<' `mappend` (fromSmallByteString name
                       `mappend` (attrs
                       `mappend` (fromAscii7Char '>'
                       `mappend` (runHtml inner mempty
                       `mappend` (fromSmallByteString "</" 
                       `mappend` (fromSmallByteString name
                       `mappend` (fromAscii7Char '>')))))))
-- By inlining this function, functions calling this (e.g. `tableHtml`) will close
-- around the `tag` variable, which ensures `tag'` is only calculated once.
{-# INLINE tag #-}

addAttr :: ByteString -> Text -> Html -> Html
addAttr key value h = Html $ \attrs ->
    runHtml h $ attrs `mappend` (fromAscii7Char ' '
                      `mappend` (fromSmallByteString key
                      `mappend` (fromSmallByteString "=\""
                      `mappend` (fromHtmlText value
                      `mappend` (fromAscii7Char '"')))))
{-# INLINE addAttr #-}

table = tag "table"
tr    = tag "tr"
td    = tag "td"

renderHtml :: Html -> BL.ByteString
renderHtml h = toLazyByteString $ runHtml h mempty

instance Monoid Html where
    mempty = Html $ \_ -> mempty
    {-# INLINE mempty #-}
    (Html h1) `mappend` (Html h2) = Html $ \attrs -> h1 attrs `mappend` h2 attrs
    {-# INLINE mappend #-}
    mconcat hs = Html $ \attrs ->
        foldr (\h k -> runHtml h attrs `mappend` k) mempty hs
    {-# INLINE mconcat #-}

bigTable :: [[Int]] -> BL.ByteString
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . showAscii7Html) r

html :: Html -> Html
html inner = 
  -- a too long string for the fairness of comparison
  rawByteString "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tag "html" inner
  
header = tag "header"
title = tag "title"
body = tag "body"
div = tag "div"
h1 = tag "h1"
h2 = tag "h2"
p = tag "p"
li = tag "li"

idA = addAttr "id"

basic :: (Text, Text, [Text]) -- ^ (Title, User, Items)
      -> BL.ByteString
basic (title', user, items) = renderHtml $ html $ mconcat
    [ header $ title $ text title'
    , body $ mconcat
        [ div $ idA "header" (h1 $ text title')
        , p $ text $ "Hello, " `mappend` user `mappend` "!"
        , p $ text $ "Hello, me!"
        , p $ text $ "Hello, world!"
        , h2 $ text "Loop"
        , mconcat $ map (li . text) items
        , idA "footer" (div $ mempty)
        ]
    ]
