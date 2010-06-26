-- | Bigtable benchmark using a constructor-based implementation.
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Monoid (..))

import Prelude hiding (div, head)
import Criterion.Main
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Exts (IsString(..))

import qualified Data.Binary.NewBuilder as NB

main :: IO ()
main = defaultMain $ 
    benchAll "bigTable"         bigTable bigTableData ++
    benchAll "basic"            basic basicData ++
    benchAll "wideTree"         wideTree wideTreeData ++
    benchAll "wideTreeEscaping" wideTree wideTreeEscapingData ++
    benchAll "deepTree"         deepTree deepTreeData ++
    benchAll "manyAttributes"   manyAttributes manyAttributesData
  where
    benchRender name f x = 
      bench ("render/"++name) $ nf (L.length . renderHtml . f) x

    benchFlatten name f x = 
      bench ("flatten/"++name) $ nf (\a -> length (flattenHtml (f a) [])) x

    benchFlattenEncode name f x = 
      bench ("flatten+encode/"++name) $ nf (L.length . flattenAndEncode . f) x

    benchAll n f x = 
      [benchFlatten n f x, benchFlattenEncode n f x, benchRender n f x]

------------------------------------------------------------------------------
-- Data for benchmarks
------------------------------------------------------------------------------

rows :: Int
rows = 1000

bigTableData :: [[Int]]
bigTableData = replicate rows [1..10]
{-# NOINLINE bigTableData #-}

manyAttributesData :: [String]
manyAttributesData = wideTreeData

basicData :: (String, String, [String])
basicData = ("Just a test", "joe", items)
{-# NOINLINE basicData #-}

items :: [String]
items = map (("Number " `mappend`) . show) [1 .. 14]
{-# NOINLINE items #-}

wideTreeData :: [String]
wideTreeData = take 5000 $
    cycle ["λf.(λx.fxx)(λx.fxx)", "These & Those", "Foobar", "lol"]
{-# NOINLINE wideTreeData #-}

wideTreeEscapingData :: [String]
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
basic :: (String, String, [String])  -- ^ (Title, User, Items)
      -> Html                  -- ^ Result.
basic (title', user, items) =  html $
    (head $ title $ string title') <>
    (body $
        (idA "header" $ div $ (h1 $ string title')) <>
        (p $ "Hello, " `mappend` string user `mappend` string "!") <>
        (p $ "Hello, me!") <>
        (p $ "Hello, world!") <>
        (h2 $ "loop") <>
        (mconcat $ map (li . string) items) <>
        (idA "footer" $ div mempty))

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [String]  -- ^ Text to create a tree from.
         -> Html    -- ^ Result.
wideTree = div . mconcat . map ((idA "foo" . p) . string)

-- | Create a very deep tree with the specified tags.
--
deepTree :: [Html -> Html]  -- ^ List of parent elements to nest.
         -> Html                -- ^ Result.
deepTree = ($ string "deep") . foldl1 (.)

-- | Create an element with many attributes.
--
manyAttributes :: [String]  -- ^ List of attribute values.
               -> Html    -- ^ Result.
manyAttributes = foldl setAttribute img
  where
    setAttribute html value = idA (HaskellString value) html
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
          | AddAttribute StaticMultiString -- ^ Key with ="
                         ChoiceString  -- ^ Value
                         Html          -- ^ Html that takes attribute
          | Empty                      -- ^ Empty Html

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

head :: Html -> Html
head = Parent "<head" "</head>"
{-# INLINE head #-}

div :: Html -> Html
div = Parent "<div" "</div>"
{-# INLINE div #-}

p :: Html -> Html
p = Parent "<p" "</p>"
{-# INLINE p #-}

h1 :: Html -> Html
h1 = Parent "<h1" "<h1/>"
{-# INLINE h1 #-}

h2 :: Html -> Html
h2 = Parent "<h2" "<h2/>"
{-# INLINE h2 #-}

li :: Html -> Html
li = Parent "<li" "<li/>"
{-# INLINE li #-}

html :: Html -> Html
html = Parent "<html" "<html/>"
{-# INLINE html #-}

title :: Html -> Html
title = Parent "<title" "<title/>"
{-# INLINE title #-}

body :: Html -> Html
body = Parent "<body" "<body/>"
{-# INLINE body #-}

h ! a = h a

(<>) :: Monoid a => a -> a -> a 
(<>) = mappend

renderHtml :: Html -> L.ByteString
renderHtml = NB.toLazyByteString . renderBuilder
{-# INLINE renderHtml #-}

renderBuilder :: Html -> NB.NewBuilder
renderBuilder = go mempty 
  where
    go attrs (Parent open close content) =
                  getNewBuilder  open
        `mappend` attrs
        `mappend` NB.utf8Char '>'
        `mappend` go mempty content
        `mappend` getNewBuilder  close
    go attrs (Leaf begin end) = 
                  getNewBuilder  begin
        `mappend` attrs
        `mappend` getNewBuilder  end
    go attrs (AddAttribute key value h) =
      go (attrs `mappend` getNewBuilder  key 
                `mappend` fromChoiceString value
                `mappend` NB.utf8Char '"')
         h
    go _ (Content content) = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go attrs Empty          = mempty
    {-# NOINLINE go #-}


{-# INLINE renderBuilder #-}

fromChoiceString :: ChoiceString -> NB.NewBuilder
fromChoiceString (StaticString   s) = getNewBuilder s
fromChoiceString (HaskellString  s) = NB.utf8CharList s
fromChoiceString (Utf8ByteString s) = NB.copyByteString s
fromChoiceString (Text           s) = error "Text not supported yet."
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
       , getNewBuilder    :: NB.NewBuilder
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
staticMultiString s = StaticMultiString s b t
  where
    b = NB.copyByteString $ S.pack $ L.unpack $
            NB.toLazyByteString $ NB.utf8CharList s
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

flattenHtml :: Html -> [HtmlPiece] -> [HtmlPiece]
flattenHtml h = go id h
  where
    go attrs c ps = case c of
      Empty           -> ps
      Content content -> content : ps
      Append h1 h2    -> go attrs h1 (go attrs h2 ps)
      Leaf begin end  -> staticPiece begin : attrs (staticPiece end : ps)
      AddAttribute key value content ->
        go (\as -> attrs (staticPiece key : value : staticDoubleQuote : as)) content ps
      Parent open close content ->
        staticPiece open : 
          attrs (staticGreater : go id content (staticPiece close : ps))
    {-# NOINLINE go #-}
{-# INLINE flattenHtml #-}

-- | This function needs to be customized such that it works directly on the
-- buffer and forms a single tight loop.
--
-- Note that for this to work perfectly the language of HtmlPieces has to be
-- adjusted.
encodeUtf8 :: [HtmlPiece] -> NB.NewBuilder
encodeUtf8 = mconcat . map fromChoiceString

flattenAndEncode :: Html -> L.ByteString
flattenAndEncode h = NB.toLazyByteString . encodeUtf8 $ flattenHtml h []

-- Remarks
--   * for the deepTree benchmark the constructor based approach is 4x faster
--     while producing the same output. Perhaps the unnecessary polymorphism is
--     hurting the Utf8Html based approach.
--
--   * It will be very interesting too see how the closure based HtmlPiece
--     generation works out in terms of flattening speed. If we are fast enough
--     there and implement a really fast encoder, then we could probably
--     achieve the speed of the Utf8Builder based approach.
--
--   * One trick which may help a bit is to get rid of the requirement for
--     providing the 'end' of a leaf tag by providing more specialized commands
--     in the HtmlPiece language. Again a trade-off between the number of
--     constructors and the number of savings has to be found. However, I guess
--     matching one constructor more is cheaper than constructing and mathcing
--     one list element more.
--
--   * Finally, note that the order of the constructors is relevant. If I
--     remember correctly, the case distinctions are done according to this
--     order. So make sure that the most used constructors come first.
--
