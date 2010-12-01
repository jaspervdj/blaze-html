-- | This is a collection of HTML benchmarks for BlazeHtml.
--
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module HtmlBenchmarks where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div, id)
import qualified Prelude as P

import Criterion.Main
import Data.ByteString.Char8 (ByteString)
import GHC.Exts (IsString, fromString)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows)
import qualified Text.Blaze.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Renderer.String as String
import qualified Text.Blaze.Renderer.Text as Text

main = defaultMain $ concatMap benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) =
        [ bench (name ++ " (Utf8)") $ nf (LB.length .  Utf8.renderHtml . f) x
        , bench (name ++ " (String)") $ nf (String.renderHtml . f) x
        , bench (name ++ " (Text)") $ nf (LT.length . Text.renderHtml . f) x
        ]

data HtmlBenchmark = forall a. HtmlBenchmark
    String       -- ^ Name.
    (a -> Html)  -- ^ Rendering function.
    a            -- ^ Data.
    Html         -- ^ Longer description.

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
        "A really deep tree (" >> showHtml deepTreeData >> " nested templates)"
    , HtmlBenchmark "manyAttributes" manyAttributes manyAttributesData $ do
        "A single element with " >> showHtml (length manyAttributesData)
        " attributes."
    ]

rows :: Int
rows = 1000

bigTableData :: [[Int]]
bigTableData = replicate rows [1..10]
{-# NOINLINE bigTableData #-}

basicData :: (String, String, [String])
basicData = ("Just a test", "joe", items)
{-# NOINLINE basicData #-}

items :: [String]
items = map (("Number " `mappend`) . show) [1 .. 14]
{-# NOINLINE items #-}

wideTreeData :: [String]
wideTreeData = take 5000 $
    cycle ["λf.(λx.fxx)(λx.fxx)", "These old days", "Foobar", "lol", "x ∈ A"]
{-# NOINLINE wideTreeData #-}

wideTreeEscapingData :: [String]
wideTreeEscapingData = take 1000 $
    cycle ["<><>", "\"lol\"", "<&>", "'>>'"]
{-# NOINLINE wideTreeEscapingData #-}

deepTreeData :: Int
deepTreeData = 1000
{-# NOINLINE deepTreeData #-}

manyAttributesData :: [String]
manyAttributesData = wideTreeData

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Html     -- ^ Result.
bigTable t = table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

-- | Render a simple HTML page with some data.
--
basic :: (String, String, [String])  -- ^ (Title, User, Items)
      -> Html                        -- ^ Result.
basic (title', user, items) = html $ do
    H.head $ title $ string title'
    body $ do
        div ! id "header" $ (h1 $ string title')
        p $ "Hello, " `mappend` string user `mappend` string "!"
        p $ "Hello, me!"
        p $ "Hello, world!"
        h2 $ "loop"
        ol $ mconcat $ map (li . string) items
        div ! id "footer" $ mempty

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [String]  -- ^ Text to create a tree from.
         -> Html      -- ^ Result.
wideTree = div . mapM_ ((p ! id "foo") . string)

-- | Create a very deep tree.
--
deepTree :: Int   -- ^ Depth of the tree.
         -> Html  -- ^ Result.
deepTree 0 = "foo"
deepTree n = p $ table $ tr $ td $ div $ deepTree (n - 1)

-- | Create an element with many attributes.
--
manyAttributes :: [String]  -- ^ List of attribute values.
               -> Html      -- ^ Result.
manyAttributes = foldl setAttribute img
  where
    setAttribute html value = html ! id (stringValue value)
    {-# INLINE setAttribute #-}
