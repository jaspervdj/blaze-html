-- | String based implementation 
-- If the export list changes, the other implementation have to be changed too
module Text.BlazeHtml.Text
<<<<<<< Updated upstream
    ( Text, pack, unpack, empty, singleton, P.hPutStr, P.putStr
    , append, map, concat, concatMap, replicate, textToBuilder
=======
    ( Text, pack, unpack, empty, singleton
    , append, map, concat, concatMap, replicate
    , hPutStr, hGetContents
>>>>>>> Stashed changes
    ) where

import qualified Data.ByteString.Lazy as LB
import qualified Prelude as P
import Prelude(String, Char, Int, (.))
import Data.Binary.Builder

type Text = String

pack :: String -> Text
pack = P.id

empty :: Text 
empty = ""

singleton :: Char -> Text
singleton = (:[])

append :: Text -> Text -> Text
append = (P.++)

map :: (Char -> Char) -> Text -> Text
map = P.map

concat :: [Text] -> Text
concat = P.concat

concatMap :: (Char -> Text) -> Text -> Text
concatMap = P.concatMap

replicate :: Int -> Text -> Text
replicate number = concat . P.replicate number

<<<<<<< Updated upstream
-- | TODO: Investigate encoding correctness. It should be UTF-8 encoded.
textToBuilder :: Text -> Builder
textToBuilder = fromLazyByteString . LB.pack
=======
hPutStr :: Handle -> Text -> IO Text
hPutStr = T.hPutStr

hGetContents :: Handle -> Text -> IO Text
hGetContents = T.hGetContents
>>>>>>> Stashed changes
