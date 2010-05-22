-- | This is a possible library implementation experiment and benchmark.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div, id, head)

import Criterion.Main
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import GHC.Exts (IsString, fromString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)

main = defaultMain
    [ benchHtml "bigTable" bigTable bigTableData
    , benchHtml "basic" basic basicData
    , benchHtml "wideTree" wideTree wideTreeData
    , benchHtml "deepTree" deepTree deepTreeData
    , benchHtml "manyAttributes" manyAttributes manyAttributesData
    ]
  where
    benchHtml name f x = bench name $ nf (BL.length . f) x

    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

    basicData :: (Text, Text, [Text])
    basicData = ("Just a test", "joe", items)
    {-# NOINLINE basicData #-}

    items :: [Text]
    items = map (("Number " `mappend`) . T.pack . show) [1 .. 14]
    {-# NOINLINE items #-}

    wideTreeData :: [Text]
    wideTreeData = take 5000 $
        cycle ["λf.(λx.fxx)(λx.fxx)", "These & Those", "Foobar", "lol"]
    {-# NOINLINE wideTreeData #-}

    deepTreeData :: [Html -> Html]
    deepTreeData = take 1000 $
        cycle [table, tr, td, p, div]
    {-# NOINLINE deepTreeData #-}

    manyAttributesData :: [Text]
    manyAttributesData = wideTreeData

bigTable :: [[Int]] -> BL.ByteString
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

basic :: (Text, Text, [Text]) -- ^ (Title, User, Items)
      -> BL.ByteString
basic (title', user, items) = renderHtml $ html $ do
    head $ title $ text title'
    body $ do
        div ! id "header" $ (h1 $ text title')
        p $ "Hello, " `mappend` text user `mappend` text "!"
        p $ "Hello, me!"
        p $ "Hello, world!"
        h2 $ "loop"
        mconcat $ map (li . text) items
        div ! id "footer" $ mempty

-- | A benchmark producing a very wide but very shallow tree.
wideTree :: [Text] -> BL.ByteString
wideTree = renderHtml . div . mapM_ ((p ! id "foo") . text)

-- | Create a very deep tree with the specified tags.
deepTree :: [Html -> Html] -> BL.ByteString
deepTree = renderHtml . ($ text "deep") . foldl1 (.)

-- | Create an element with many attributes.
manyAttributes :: [Text] -> BL.ByteString
manyAttributes = renderHtml . foldl setAttribute img
  where
    setAttribute html value = html ! id (textValue value)
    {-# INLINE setAttribute #-}
