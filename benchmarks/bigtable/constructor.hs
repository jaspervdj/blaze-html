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
    ]
  where
    benchHtml name f x = bench name $ nf (L.length . f) x

    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]        -- ^ Matrix.
         -> L.ByteString  -- ^ Result.
bigTable t = renderHtml $ table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . string . show) r

------------------------------------------------------------------------------
-- Html Constructors
------------------------------------------------------------------------------

-- Html constructors can then use StaticMultiStrings to represents begin and
-- end tag and a MultiString to represent content. Moreover, we can also take care
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
                    Html               -- ^ Content.
                    StaticMultiString  -- ^ End tag.
          | Leaf    StaticMultiString  -- ^ Leaf tag.
          | Content MultiString        -- ^ Html content.
          | List    [Html]             -- ^ Concatenation.

instance Monoid Html where
    mempty = List []
    {-# INLINE mempty #-}

    mappend (List x) (List y) = List (x `mappend` y)
    mappend (List x) y = List $ x `mappend` [y]
    mappend x (List y) = List $ x : y
    mappend x y = List $ [x, y]
    {-# INLINE mappend #-}

    mconcat = List
    {-# INLINE mconcat #-}

parent :: String -> Html -> Html
parent tag content =
    let open = "<" `mappend` tag `mappend` ">"
        openTag = staticMultiString open
        close = "</" `mappend` tag `mappend` ">"
        closeTag = staticMultiString close
    in Parent openTag content closeTag
{-# INLINE parent #-}

table :: Html -> Html
table = parent "table"
{-# INLINE table #-}

tr :: Html -> Html
tr = parent "tr"
{-# INLINE tr #-}

td :: Html -> Html
td = parent "td"
{-# INLINE td #-}

string :: String -> Html
string = Content . HaskellString
{-# INLINE string #-}

renderHtml :: Html -> L.ByteString
renderHtml = UB.toLazyByteString . renderBuilder
{-# INLINE renderHtml #-}

renderBuilder :: Html -> Utf8Builder
renderBuilder (Parent open content close) =
              (UB.unsafeFromByteString $ getUtf8ByteString open)
    `mappend` renderBuilder content
    `mappend` (UB.unsafeFromByteString $ getUtf8ByteString close)
renderBuilder (Leaf _) = undefined
renderBuilder (Content content) = case content of
    StaticString   s -> UB.unsafeFromByteString $ getUtf8ByteString s
    HaskellString  s -> UB.fromString s
    Utf8ByteString s -> UB.unsafeFromByteString s
    Text           s -> UB.fromText s
renderBuilder (List hs) = mconcat $ map renderBuilder hs
{-# INLINE renderBuilder #-}

-- | The key ingredient is a string representation that supports all possible
-- outputs well. However, we cannot care for *all possible* output formats, but
-- we can care for all known output formats.
--
-- Note that I'm using a lazy ByteString where we should probably use a
-- Utf8Builder. The same holds in some cases for Text, which may eventually
-- better be replaced by a builder. 
data StaticMultiString = StaticMultiString
       { getHaskellString  :: String  
       , getUtf8ByteString :: S.ByteString 
       , getText           :: Text
       }

-- | A string denoting input from different string representations.
data MultiString = 
     StaticString   StaticMultiString -- ^ Input from a set of precomputed
                                      --   representations.
   | HaskellString  String            -- ^ Input from a Haskell String
   | Utf8ByteString S.ByteString      -- ^ Input from a Utf8 encoded bytestring
   | Text           Text            -- ^ Input from a Text value


-- | A static string that is built once and used many times. Here, we could
-- also use the `cached` (optimizePiece) construction for our builder.
staticMultiString :: String -> StaticMultiString
staticMultiString s = StaticMultiString s (T.encodeUtf8 t) t
  where
    t = T.pack s
{-# INLINE staticMultiString #-}

-- Overloaded strings support
-----------------------------

instance IsString StaticMultiString where
  fromString = staticMultiString

instance IsString MultiString where
  fromString = HaskellString
