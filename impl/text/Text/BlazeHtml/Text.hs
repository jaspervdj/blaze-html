-- | 'Data.Text' based implementation 
-- If the export list changes, the other implementation have to be changed too
module Text.BlazeHtml.Text
    ( Text, T.pack, T.unpack, T.empty, T.singleton
    , T.append, T.map, T.concat, T.concatMap
    , T.foldr
    , T.replicate
    , TIO.putStr
    , TIO.hPutStr
    , textToBuilder
    , T.hGetContents
    ) where

import Data.Binary.Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO as T

type Text = T.Text

textToBuilder :: Text -> Builder
textToBuilder = fromByteString . E.encodeUtf8
