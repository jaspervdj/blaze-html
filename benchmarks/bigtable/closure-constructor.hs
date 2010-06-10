-- | Bigtable benchmark using a constructor-based implementation.
--
{-# LANGUAGE OverloadedStrings #-}

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
    benchAll "bigTable" bigTable bigTableData
  where
    benchFlatten name f x = 
      bench ("flatten/"++name) $ nf (length . flattenHtml . f) x

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

------------------------------------------------------------------------------
-- Html Constructors
------------------------------------------------------------------------------

-- Html constructors can then use StaticMultiStrings to represents begin and
-- end tag and a ChoiceString to represent content. Moreover, we can also take care
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
    { unHtml :: ([HtmlPiece] -> [HtmlPiece]) -> [HtmlPiece] -> [HtmlPiece]
    }

instance Monoid Html where
    mempty = Html id
    {-# INLINE mempty #-}

    mappend (Html f) (Html g) = Html $ \x -> f x . g x
    {-# INLINE mappend #-}

    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

instance IsString Html where
    fromString s = Html $ \_ k -> fromString s : k
    {-# INLINE fromString #-}

type Attribute = ChoiceString -> Html -> Html

parent :: HtmlPiece -> HtmlPiece -> Html -> Html
parent open close (Html inner) = Html $ \attrs k ->
    open : attrs (staticGreater : inner id (close : k))
{-# INLINE parent #-}

table :: Html -> Html
table = parent "<table" "</table>"
{-# INLINE table #-}

tr :: Html -> Html
tr = parent "<tr" "</tr>"
{-# INLINE tr #-}

td :: Html -> Html
td = parent "<td" "</td>"
{-# INLINE td #-}

string :: String -> Html
string s = Html $ \_ k -> (HaskellString s : k)
{-# INLINE string #-}

flattenHtml :: Html -> [HtmlPiece]
flattenHtml (Html f) = f id []
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
-- expensive.
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
encodeUtf8 :: [HtmlPiece] -> L.ByteString
encodeUtf8 = UB.toLazyByteString . go
  where
  go []     = mempty
  go (x:xs) = fromChoiceString x `mappend` go xs
{-# INLINE encodeUtf8 #-}

flattenAndEncode :: Html -> L.ByteString
flattenAndEncode = encodeUtf8 . flattenHtml
{-# INLINE flattenAndEncode #-}

fromChoiceString :: ChoiceString -> Utf8Builder
fromChoiceString (StaticString   s) = UB.unsafeFromByteString $ getByteString s
fromChoiceString (HaskellString  s) = UB.fromString s
fromChoiceString (Utf8ByteString s) = UB.unsafeFromByteString s
fromChoiceString (Text           s) = UB.fromText s
{-# INLINE fromChoiceString #-}

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
data ChoiceString =
     StaticString   StaticMultiString -- ^ Input from a set of precomputed
                                      --   representations.
   | HaskellString  String            -- ^ Input from a Haskell String
   | Text           Text              -- ^ Input from a Text value
   | Utf8ByteString S.ByteString      -- ^ Input from a Utf8 encoded bytestring

-- Overloaded strings support
-----------------------------

instance IsString StaticMultiString where
  fromString = staticMultiString
  {-# INLINE fromString #-}

instance IsString ChoiceString where
  fromString = StaticString . staticMultiString
  {-# INLINE fromString #-}

-- Monad support
----------------

------------------------------------------------------------------------------
-- Constructor Flattening
------------------------------------------------------------------------------

type HtmlPiece = ChoiceString

staticPiece :: StaticMultiString -> HtmlPiece
staticPiece = StaticString
{-# INLINE staticPiece #-}

staticDoubleQuote :: ChoiceString
staticDoubleQuote = StaticString "\""

staticGreater :: ChoiceString
staticGreater = StaticString ">"
