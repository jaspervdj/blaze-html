-- | This is a possible library implementation experiment and benchmark.
--
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Utf8Html where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div, id)
import qualified Prelude as P

import Criterion.Main
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import GHC.Exts (IsString, fromString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows)

main = defaultMain $ map benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) = bench name $ nf (LB.length . f) x

data HtmlBenchmark = forall a b. HtmlBenchmark
    String                -- ^ Name.
    (a -> LB.ByteString)  -- ^ Rendering function.
    a                     -- ^ Data.
    (Html b)              -- ^ Longer description.

-- | List containing all benchmarks.
--
benchmarks :: [HtmlBenchmark]
benchmarks =
    [ HtmlBenchmark "bigTable" bigTable bigTableData $
        let h = showHtml $ length bigTableData
            w = showHtml $ length $ P.head bigTableData
        in "Rendering of a big (" >> h >> "x" >> w >> ") HTML table"
    , HtmlBenchmark "basic" basic basicData
        "A simple, small basic template with a few holes to fill in"
    , HtmlBenchmark "wideTree" wideTree wideTreeData $
        "A very wide tree (" >> showHtml (length wideTreeData) >> " elements)"
    , HtmlBenchmark "wideTreeEscaping" wideTree wideTreeEscapingData $ do
        "A very wide tree (" >> showHtml (length wideTreeData) >> " elements)"
        " with lots of escaping"
    , HtmlBenchmark "deepTree" deepTree deepTreeData $ do
        "A really deep tree (" >> showHtml (length deepTreeData) >> " nested"
        " elements)"
    , HtmlBenchmark "manyAttributes" manyAttributes manyAttributesData $ do
        "A single element with " >> showHtml (length manyAttributesData)
        " attributes."
    ]

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

wideTreeEscapingData :: [Text]
wideTreeEscapingData = take 1000 $
    cycle ["<><>", "\"lol\"", "<&>", "'>>'"]
{-# NOINLINE wideTreeEscapingData #-}

deepTreeData :: [Html a -> Html a]
deepTreeData = take 1000 $
    cycle [table, tr, td, p, div]
{-# NOINLINE deepTreeData #-}

manyAttributesData :: [Text]
manyAttributesData = wideTreeData

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]        -- ^ Matrix.
         -> LB.ByteString  -- ^ Result.
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

-- | Render a simple HTML page with some data.
--
basic :: (Text, Text, [Text])  -- ^ (Title, User, Items)
      -> LB.ByteString         -- ^ Result.
basic (title', user, items) = renderHtml $ html $ do
    H.head $ title $ text title'
    body $ do
        div ! id "header" $ (h1 $ text title')
        p $ "Hello, " `mappend` text user `mappend` text "!"
        p $ "Hello, me!"
        p $ "Hello, world!"
        h2 $ "loop"
        mconcat $ map (li . text) items
        div ! id "footer" $ mempty

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [Text]         -- ^ Text to create a tree from.
         -> LB.ByteString  -- ^ Result.
wideTree = renderHtml . div . mapM_ ((p ! id "foo") . text)

-- | Create a very deep tree with the specified tags.
--
deepTree :: [Html a -> Html a]  -- ^ Elements to nest.
          -> LB.ByteString       -- ^ Result.
deepTree = renderHtml . ($ "foo") . foldl1 (.)
{-# NOINLINE deepTree #-}

-- | Create an element with many attributes.
--
manyAttributes :: [Text]         -- ^ List of attribute values.
               -> LB.ByteString  -- ^ Result.
manyAttributes = renderHtml . foldl setAttribute img
  where
    setAttribute html value = html ! id (textValue value)
    {-# INLINE setAttribute #-}
