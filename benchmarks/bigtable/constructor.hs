-- | Bigtable benchmark using a constructor-based implementation.
--
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (Monoid (..))

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
main = defaultMain
    [ benchHtml "bigTable" bigTable bigTableData
    , benchHtml "manyAttributes" manyAttributes manyAttributesData
    ]
  where
    benchHtml name f x = bench name $ nf (L.length . f) x

    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

    wideTreeData :: [Text]
    wideTreeData = take 5000 $
        cycle ["λf.(λx.fxx)(λx.fxx)", "These & Those", "Foobar", "lol"]
    {-# NOINLINE wideTreeData #-}
    
    manyAttributesData :: [Text]
    manyAttributesData = wideTreeData

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]       -- ^ Matrix.
         -> L.ByteString  -- ^ Result.
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

-- | Create an element with many attributes.
--
manyAttributes :: [Text]         -- ^ List of attribute values.
               -> L.ByteString  -- ^ Result.
manyAttributes = renderHtml . foldl setAttribute img
  where
    setAttribute html value = idA (Text value) html
    {-# INLINE setAttribute #-}

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

data Html = Parent  StaticMultiString  -- ^ Open tag.
                    StaticMultiString  -- ^ End tag.
                    Html               -- ^ Content.
          | Leaf    StaticMultiString  -- ^ Begin of leaf tag.
                    StaticMultiString  -- ^ End of leaf tag.
          | Content ChoiceString       -- ^ Html content.
          | Append  Html Html          -- ^ Concatenation.
          | Empty                      -- ^ Empty Html
          | AddAttribute StaticMultiString -- ^ Key with ="
                         ChoiceString  -- ^ Value
                         Html          -- ^ Html that takes attribute

instance Monoid Html where
    mempty = Empty
    {-# INLINE mempty #-}

    mappend = Append
    {-# INLINE mappend #-}

    mconcat = foldr Append Empty
    {-# INLINE mconcat #-}

instance IsString Html where
    fromString = string
    {-# INLINE fromString #-}

type Attribute = ChoiceString -> Html -> Html

idA :: Attribute
idA = AddAttribute " id=\""
{-# INLINE idA #-}

parent :: String -> Html -> Html
parent tag =
    let open = "<" `mappend` tag
        openTag = staticMultiString open
        close = "</" `mappend` tag `mappend` ">"
        closeTag = staticMultiString close
    in Parent openTag closeTag
{-# INLINE parent #-}

table :: Html -> Html
table = Parent "<table" "</table>"
{-# INLINE table #-}

tr :: Html -> Html
tr = Parent "<tr" "</tr>"
{-# INLINE tr #-}

td :: Html -> Html
td = Parent "<td" "</td>"
{-# INLINE td #-}

img :: Html
img = Leaf "<img" ">"
{-# INLINE img #-}

string :: String -> Html
string = Content . HaskellString
{-# INLINE string #-}

text :: Text -> Html
text = Content . Text
{-# INLINE text #-}

renderHtml :: Html -> L.ByteString
renderHtml = UB.toLazyByteString . renderBuilder
{-# INLINE renderHtml #-}

renderBuilder :: Html -> Utf8Builder
renderBuilder = go mempty 
  where
    go attrs (Parent open close content) =
                  getUtf8Builder open
        `mappend` attrs
        `mappend` UB.fromChar '>'
        `mappend` go mempty content
        `mappend` getUtf8Builder close
    go attrs (Leaf begin end) = 
                  getUtf8Builder begin
        `mappend` attrs
        `mappend` getUtf8Builder end
    go attrs (AddAttribute key value h) =
      go (attrs `mappend` getUtf8Builder key 
                `mappend` fromMultiString value
                `mappend` UB.fromChar '"')
         h
    go _ (Content content) = fromMultiString content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go attrs Empty          = mempty
    {-# NOINLINE go #-}

    fromMultiString (StaticString   s) = getUtf8Builder s
    fromMultiString (HaskellString  s) = UB.fromString s
    fromMultiString (Utf8ByteString s) = UB.unsafeFromByteString s
    fromMultiString (Text           s) = UB.fromText s
    {-# INLINE fromMultiString #-}

{-# INLINE renderBuilder #-}

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
   | Utf8ByteString S.ByteString      -- ^ Input from a Utf8 encoded bytestring
   | Text           Text            -- ^ Input from a Text value

-- Overloaded strings support
-----------------------------

instance IsString StaticMultiString where
  fromString = staticMultiString

instance IsString ChoiceString where
  fromString = HaskellString
