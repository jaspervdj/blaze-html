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
    -- benchFlatten name f x = 
      -- bench ("flatten/"++name) $ nf (\a -> length (flattenHtml (f a) [])) x

    benchFlattenEncode name f x = 
      bench ("flatten+encode/"++name) $ nf (L.length . flattenAndEncode . f) x

    benchAll n f x = [benchFlattenEncode n f x]

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
    fromString = string
    {-# INLINE fromString #-}

type Attribute = ChoiceString -> Html -> Html

parent :: String -> String -> Html -> Html
parent open close (Html inner) =
    let openTag = StaticString $ staticMultiString open
        closeTag = StaticString $ staticMultiString close
        g :: ([HtmlPiece] -> [HtmlPiece]) -> [HtmlPiece] -> [HtmlPiece]
        g attrs k = openTag : attrs (staticGreater : inner id (closeTag : k))
    in Html $ \attrs f -> g attrs f
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
flattenHtml (Html f) = f id mempty
{-# INLINE flattenHtml #-}

encodeUtf8 :: [HtmlPiece] -> L.ByteString
encodeUtf8 = UB.toLazyByteString . mconcat . map fromChoiceString
{-# INLINE encodeUtf8 #-}

flattenAndEncode :: Html -> L.ByteString
flattenAndEncode = encodeUtf8 . flattenHtml
{-# INLINE flattenAndEncode #-}

fromChoiceString :: ChoiceString -> Utf8Builder
fromChoiceString (StaticString   s) = getUtf8Builder s
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
       , getUtf8Builder   :: Utf8Builder
       , getText          :: Text
       }

instance Monoid StaticMultiString where
    mempty = StaticMultiString mempty mempty mempty
    {-# INLINE mempty #-}
    mappend (StaticMultiString x1 y1 z1) (StaticMultiString x2 y2 z2) =
        StaticMultiString (x1 `mappend` x2)
                          (y1 `mappend` y2)
                          (z1 `mappend` z2)
    {-# INLINE mappend #-}

-- | A static string that is built once and used many times. Here, we could
-- also use the `cached` (optimizePiece) construction for our builder.
staticMultiString :: String -> StaticMultiString
staticMultiString s = StaticMultiString s (UB.optimizePiece $ UB.fromText t) t
  where
    t = T.pack s
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

instance IsString ChoiceString where
  fromString = HaskellString

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
