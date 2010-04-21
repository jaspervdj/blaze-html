-- | This is a possible library implementation experiment and benchmark.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div, id)

import Criterion.Main
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import GHC.Exts (IsString, fromString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Text.Blaze.Internal.Utf8Builder
import Text.Blaze
import Text.Blaze.Html.Strict
import Text.Blaze.Html.Strict.Attributes

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

bigTable :: [[Int]] -> BL.ByteString
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . showHtml) r

hello1, hello2, hello3, loop :: ByteString
hello1 = "Hello, "
hello2 = "Hello, me!"
hello3 = "Hello, world!"
loop   = "Loop"

static = rawByteString
{-# INLINE static #-}

basic :: (Text, Text, [Text]) -- ^ (Title, User, Items)
      -> BL.ByteString
basic (title', user, items) = renderHtml $ html $ mconcat
    [ header $ title $ text title'
    , body $ mconcat
        [ div ! id "header" $ (h1 $ text title')
        , p $ static hello1 `mappend` text user `mappend` text "!"
        , p $ static hello2
        , p $ static hello3
        , h2 $ static loop
        , mconcat $ map (li . text) items
        , div ! id "footer" $ mempty
        ]
    ]
