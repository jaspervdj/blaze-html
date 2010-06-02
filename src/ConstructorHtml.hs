-- Author: Simon Meier <simon.meier@inf.ethz.ch>
-- Date: June 2nd, 2010
--
-- Purpose: Explains a constructor based approach for getting the flexibility
-- of an interpreter together with the speed benefit of using literal strings
-- preconverted to the output format for maximal rendering speed.
--
-- @Jasper: Note that this way we won't need to enumerate all constructors.
-- Instead we just preload the constructors for the basic Html combinators
-- (parent, leaf, ...) with the correctly represented strings.

{-# LANGUAGE OverloadedStrings #-}
module ConstructorHtml where

import qualified Data.ByteString as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Exts (IsString(..))

-- | The key ingredient is a string representation that supports all possible
-- outputs well. However, we cannot care for *all possible* output formats, but
-- we can care for all known output formats.
--
-- Note that I'm using a lazy ByteString where we should probably use a
-- Utf8Builder. The same holds in some cases for Text, which may eventually
-- better be replaced by a builder. 
data StaticMultiString = StaticMultiString
       { getHaskellString  :: String  
       , getUtf8ByteString :: LB.ByteString 
       , getText           :: T.Text
       }

-- | A string denoting input from different string representations.
data MultiString = 
     StaticString   StaticMultiString -- ^ Input from a set of precomputed
                                      --   representations.
   | HaskellString  String            -- ^ Input from a Haskell String
   | Utf8ByteString LB.ByteString     -- ^ Input from a Utf8 encoded bytestring
   | Text           T.Text            -- ^ Input from a Text value


-- | A static string that is built once and used many times. Here, we could
-- also use the `cached` (optimizePiece) construction for our builder.
staticMultiString :: String -> StaticMultiString
staticMultiString s = StaticMultiString s (T.encodeUtf8 t) t
  where t = T.pack s

-- Input creation for a dynamic multi string
--------------------------------------------

staticString :: String -> MultiString
staticString = StaticString . staticMultiString

string :: String -> MultiString
string = HaskellString

utf8ByteString :: LB.ByteString -> MultiString
utf8ByteString = Utf8ByteString

text :: T.Text -> MultiString
text = Text

-- Overloaded strings support
-----------------------------

instance IsString StaticMultiString where
  fromString = staticMultiString

instance IsString MultiString where
  fromString = staticString

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
