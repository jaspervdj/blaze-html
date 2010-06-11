-- | Bigtable benchmark using a constructor-based implementation.
--
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Data.Monoid (Monoid (..))

import Prelude hiding (div, head)
import Criterion.Main
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Exts (IsString(..))

import Text.Blaze.Internal.Utf8Builder (Utf8Builder)
import qualified Text.Blaze.Internal.Utf8Builder as UB

main :: IO ()
main = defaultMain $
    benchAll "bigTable"         bigTable bigTableData ++
    benchAll "basic"            basic basicData ++
    benchAll "wideTree"         wideTree wideTreeData ++
    benchAll "wideTreeEscaping" wideTree wideTreeEscapingData ++
    benchAll "deepTree"         deepTree deepTreeData ++
    benchAll "manyAttributes"   manyAttributes manyAttributesData
  where
    benchFlatten name f x =
      bench ("flatten/"++name) $ nf (pieceSize . flattenHtml . f) x

    benchFlattenEncode name f x =
      bench ("flatten+encode/"++name) $ nf (L.length . flattenAndEncode . f) x

    benchAll n f x = [benchFlatten n f x, benchFlattenEncode n f x]

------------------------------------------------------------------------------
-- Data for benchmarks
------------------------------------------------------------------------------

rows :: Int
rows = 1000

bigTableData :: [[Int]]
bigTableData = replicate rows [1..10]
{-# NOINLINE bigTableData #-}

manyAttributesData :: [Text]
manyAttributesData = wideTreeData

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

deepTreeData :: [Html -> Html]
deepTreeData = take 1000 $
    cycle [table, tr, td, p, div]
{-# NOINLINE deepTreeData #-}

------------------------------------------------------------------------------
-- Html generators
------------------------------------------------------------------------------

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]        -- ^ Matrix.
         -> Html  -- ^ Result.
bigTable t = table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

-- | Render a simple HTML page with some data.
--
basic :: (Text, Text, [Text])  -- ^ (Title, User, Items)
      -> Html                  -- ^ Result.
basic (title', user, items) =  html $
    (head $ title $ text title') <>
    (body $
        (idA "header" $ div $ (h1 $ text title')) <>
        (p $ "Hello, " `mappend` text user `mappend` text "!") <>
        (p $ "Hello, me!") <>
        (p $ "Hello, world!") <>
        (h2 $ "loop") <>
        (mconcat $ map (li . text) items) <>
        (idA "footer" $ div mempty))

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [Text]  -- ^ Text to create a tree from.
         -> Html    -- ^ Result.
wideTree = div . mconcat . map ((idA "foo" . p) . text)

-- | Create a very deep tree with the specified tags.
--
deepTree :: [Html -> Html]  -- ^ List of parent elements to nest.
         -> Html                -- ^ Result.
deepTree = ($ text "deep") . foldl1 (.)

-- | Create an element with many attributes.
--
manyAttributes :: [Text]  -- ^ List of attribute values.
               -> Html    -- ^ Result.
manyAttributes = foldl setAttribute img
  where
    setAttribute html value = idA (textValue value) html
    {-# INLINE setAttribute #-}

------------------------------------------------------------------------------
-- Html Constructors
------------------------------------------------------------------------------

-- Html constructors can then use StaticMultiStrings to represents begin and
-- end tag and a HtmlPiece to represent content. Moreover, we can also take care
-- to precompute the escaping where possible using a similar construction.
--
-- Hence, we will have a small algebraic Data Type for Html constructors and
-- nice, cached string representations to help the interpreters (i.e.,
-- renderers).
--
-- We could also use the same trick to cover different renderers by preparing
-- the right strings for the tags up front.
--
-- Note that for the interpreter to be as fast as possible, I think the
-- attributes should be tracked in a recursive argument instead of in a
-- closure; i.e. the intepreter would be based directly on HtmlByteString.
-- Moreover, I think a tradeoff has to be found between the number of
-- constructors and the nesting involved. In the end, we just want that the
-- overhead per combinator is as low as possible.
--
-- However, I think this constructor based approch could be quite efficient, as
-- we can still ensure that all the copying and encoding happens in not
-- too-small units and non-redundant for literal strings. Moreover, we trade
-- the construction of closures against the matching with a small set of
-- constructors. Perhaps this is a fair trade.
--
-- Looking forward to the first results for the BigTable benchmark :-)

newtype Html = Html
    { unHtml :: (HtmlPiece -> HtmlPiece) -> HtmlPiece -> HtmlPiece
    }

instance Monoid Html where
    mempty = Html $ const id
    {-# INLINE mempty #-}

    mappend (Html f) (Html g) = Html $ \x -> f x . g x
    {-# INLINE mappend #-}

    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

instance IsString Html where
    fromString s = Html $ \_ k -> StaticString (fromString s) k
    {-# INLINE fromString #-}

type Attribute = AttributeValue -> Html -> Html

newtype AttributeValue = AttributeValue
    { unAttributeValue :: HtmlPiece -> HtmlPiece
    }

instance IsString AttributeValue where
    fromString s = AttributeValue $ \k -> StaticString (staticMultiString s) k

parent :: StaticMultiString -> StaticMultiString -> Html -> Html
parent open close = \inner -> Html $ \attrs k ->
    StaticString open (attrs (staticGreater (unHtml inner id (StaticString close k))))
{-# INLINE parent #-}

leaf :: StaticMultiString -> Html
leaf open = Html $ \attrs k ->
    StaticString open (attrs (staticLeafEnd k))
{-# INLINE leaf #-}

attribute :: StaticMultiString -> Attribute
attribute key = \(AttributeValue value) h -> Html $ \attrs k ->
    unHtml h (\j -> attrs (StaticString key (value $ staticDoubleQuote j))) k
{-# INLINE attribute #-}

table :: Html -> Html
table = parent "<table" "</table>"
{-# INLINE table #-}

tr :: Html -> Html
tr = parent "<tr" "</tr>"
{-# INLINE tr #-}

td :: Html -> Html
td = parent "<td" "</td>"
{-# INLINE td #-}

idA :: Attribute
idA = attribute " id=\""
{-# INLINE idA #-}

img :: Html
img = leaf "<img"
{-# INLINE img #-}

head :: Html -> Html
head = parent "<head" "</head>"
{-# INLINE head #-}

div :: Html -> Html
div = parent "<div" "</div>"
{-# INLINE div #-}

p :: Html -> Html
p = parent "<p" "</p>"
{-# INLINE p #-}

h1 :: Html -> Html
h1 = parent "<h1" "<h1/>"
{-# INLINE h1 #-}

h2 :: Html -> Html
h2 = parent "<h2" "<h2/>"
{-# INLINE h2 #-}

li :: Html -> Html
li = parent "<li" "<li/>"
{-# INLINE li #-}

html :: Html -> Html
html = parent "<html" "<html/>"
{-# INLINE html #-}

title :: Html -> Html
title = parent "<title" "<title/>"
{-# INLINE title #-}

body :: Html -> Html
body = parent "<body" "<body/>"
{-# INLINE body #-}

h ! a = h a

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

string :: String -> Html
string s = Html $ \_ -> HaskellString s
{-# INLINE string #-}

text :: Text -> Html
text t = Html $ \_ -> Text t
{-# INLINE text #-}

textValue :: Text -> AttributeValue
textValue t = AttributeValue $ \k -> Text t k

flattenHtml :: Html -> HtmlPiece
flattenHtml (Html f) = f id EmptyPiece
{-# INLINE flattenHtml #-}

-- This is the key function. It needs to be as fast as possible. Use ghc-core to
-- investigate its implementation. Ideally we would have one straight loop
-- gathering the input and filling the buffer. I guess that using the builder
-- is hurting us here a bit. Instead this should be implemented directly on the
-- representation of a builder. Such that it yields an Utf8Builder in the end,
-- but internally uses all sorts of tricks to get the best speed possible.
--
-- Already the small change from idiomatic haskell to a tail-recursive function
-- brought a speed-up of 16% on my machine. I assume that there's quite some
-- more possible. Moreoever, note that the bigtable benchmark is especially
-- tough when using this intermediate representation, because the each piece is
-- short and takes only little encoding time. Hence, amortizing the cost of
-- building and deconstruction the intermediate list is tough. However, I dare
-- say that this amortization is simpler for more realistic benchmarks. Hence,
-- the additional flexibility of this intermediate representation is not too
-- expensive. Hence, it would be good to compare the different implementations
-- on all benchmarks and not just the bigtable one.
--
-- This flexibility is actually *very* high. By choosing the right HtmlPiece
-- language not only pretty printing, but also lossy formats become supportable
-- with good speed again. Static strings are preescaped for all possible
-- encodings, while dynamic strings are escaped accordingly. I'm not sure if we
-- want that, but this discussion
--
--   http://www.reddit.com/r/haskell/comments/cbzbo/optimizing_hamlet/c0rjo9r?context=3
--
-- points towards a direction that this could once be needed. Currently, I'm
-- happy with the fact that we have a possible way of efficiently supporting
-- UTF-8, Text, and String without any type system hacks. Just a nice single
-- datatype catering for our needs :-)
--
-- So the next step would be to see how you can get encodeUtf8 super fast while
-- still keeping it lazy.
--
-- Update: Nice, Removing the another indirection allowed me to shave off
-- another 16% from the original HtmlPiece based approach. Flattening takes
-- still almost the same time, but now the required data is directly available
-- :-)
encodeUtf8 :: HtmlPiece -> L.ByteString
encodeUtf8 = UB.toLazyByteString . go
  where
  go EmptyPiece           = mempty
  go (StaticString   s p) = (UB.unsafeFromByteString $ getByteString s) `mappend` go p
  go (HaskellString  s p) = (UB.fromString s) `mappend` go p
  go (Utf8ByteString s p) = (UB.unsafeFromByteString s) `mappend` go p
  go (Text           s p) = (UB.fromText s) `mappend` go p
  go (Append         s p) = go s `mappend` go p
{-# INLINE encodeUtf8 #-}

flattenAndEncode :: Html -> L.ByteString
flattenAndEncode = encodeUtf8 . flattenHtml
{-# INLINE flattenAndEncode #-}


-- | The key ingredient is a string representation that supports all possible
-- outputs well. However, we cannot care for *all possible* output formats, but
-- we can care for all known output formats.
--
-- Note that I'm using a lazy ByteString where we should probably use a
-- Utf8Builder. The same holds in some cases for Text, which may eventually
-- better be replaced by a builder.
data StaticMultiString = StaticMultiString
       { getHaskellString :: String
       , getByteString    :: S.ByteString
       , getText          :: Text
       }

{-
instance Monoid StaticMultiString where
    mempty = StaticMultiString mempty mempty mempty
    {-# INLINE mempty #-}
    mappend (StaticMultiString x1 y1 z1) (StaticMultiString x2 y2 z2) =
        StaticMultiString (x1 `mappend` x2)
                          (y1 `mappend` y2)
                          (z1 `mappend` z2)
    {-# INLINE mappend #-}
-}

-- | A static string that is built once and used many times. Here, we could
-- also use the `cached` (optimizePiece) construction for our builder.
staticMultiString :: String -> StaticMultiString
staticMultiString s = StaticMultiString s bs t
  where
    bs = S.pack $ L.unpack $ UB.toLazyByteString $ UB.fromString s
    t  = T.pack s
{-# INLINE staticMultiString #-}

-- | A string denoting input from different string representations.
data HtmlPiece =
     -- SM: Perhaps it would help to remove an additional indirection by
     -- inlining the different required output formats in the constructor for
     -- static string. However, I'm not sure. I tried doing that by using
     -- making the first argument strict, but that didn't help at all.
     --
     -- Make this low priority.
     --
     -- Before trying out such optimizations, I would rather make the
     -- implementation of encodeUtf8 faster for the existing HtmlPiece datatype
     -- or a similar one.
     --
     StaticString   StaticMultiString HtmlPiece -- ^ Input from a set of precomputed
                                                --   representations.
   | HaskellString  String            HtmlPiece -- ^ Input from a Haskell String
   | Text           Text              HtmlPiece -- ^ Input from a Text value
   | Utf8ByteString S.ByteString      HtmlPiece -- ^ Input from a Utf8 encoded bytestring
   | Append         HtmlPiece         HtmlPiece -- ^ Appending of two pieces.
   | EmptyPiece                                 -- ^ An empty html piece.

-- | Compute the size of the Html piece; i.e. the number of constructors. This
-- can be used to measure the time for 'flattening' it.
pieceSize :: HtmlPiece -> Int
pieceSize = go 0
  where
  go !s p = case p of
    StaticString   _ p -> go (1 + s) p
    HaskellString  _ p -> go (1 + s) p
    Text           _ p -> go (1 + s) p
    Utf8ByteString _ p -> go (1 + s) p
    EmptyPiece         -> s


-- Overloaded strings support
-----------------------------

instance IsString StaticMultiString where
  fromString = staticMultiString
  {-# INLINE fromString #-}


-- Monad support
----------------

------------------------------------------------------------------------------
-- Constructor Flattening
------------------------------------------------------------------------------

staticDoubleQuote :: HtmlPiece -> HtmlPiece
staticDoubleQuote = StaticString "\""

staticGreater :: HtmlPiece -> HtmlPiece
staticGreater = StaticString ">"

staticLeafEnd :: HtmlPiece -> HtmlPiece
staticLeafEnd = StaticString " />"
