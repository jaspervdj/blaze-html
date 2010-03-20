-- | String based implementation 
-- If the export list changes, the other implementation have to be changed too
module Text.BlazeHtml.Text
    ( Text, pack, empty, singleton
    , append, map, concat, concatMap, replicate
    ) where

import qualified Prelude as P
import Prelude(String, Char, Int, (.))

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
