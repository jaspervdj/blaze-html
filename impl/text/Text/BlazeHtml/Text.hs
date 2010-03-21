-- | 'Data.Text' based implementation 
-- If the export list changes, the other implementation have to be changed too
module Text.BlazeHtml.Text
    ( Text, T.pack, T.unpack, T.empty, T.singleton
    , T.append, T.map, T.concat, T.concatMap
<<<<<<< Updated upstream
    , T.replicate
    , TIO.putStr
    , TIO.hPutStr
    , textToBuilder
=======
    , T.replicate, T.hPutStr, T.hGetContents
>>>>>>> Stashed changes
    ) where

import Data.Binary.Builder
import qualified Data.Text as T
<<<<<<< Updated upstream
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
=======
import qualified Data.Text.IO as T
>>>>>>> Stashed changes

type Text = T.Text

textToBuilder :: Text -> Builder
textToBuilder = fromByteString . E.encodeUtf8
