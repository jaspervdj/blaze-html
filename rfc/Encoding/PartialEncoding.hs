module Encoding.PartialEncoding where

import Data.Monoid (Monoid (..))

import qualified Data.Text as T

import Internal.Html

data EncodingInfo s = EncodingInfo
    { eiTag         :: !s
    , eiUnencodable :: !(Char -> Bool)
    }

newtype PartialEncoding s = PartialEncoding
    { runPartialEncoding :: EncodingInfo s -> (Char -> s) -> s
    }

instance Monoid s => Monoid (PartialEncoding s) where
    mempty          = PartialEncoding $ \ _ _     -> mempty
    s1 `mappend` s2 = PartialEncoding $ \ei subst -> 
        runPartialEncoding s1 ei subst `mappend` runPartialEncoding s2 ei subst
    mconcat ss      = PartialEncoding $ \ei subst -> 
        mconcat . map (\s -> runPartialEncoding s ei subst) $ ss

instance UnicodeSequence s => UnicodeSequence (PartialEncoding s) where
    unicodeChar c   = PartialEncoding $ \ei subst ->
        if eiUnencodable ei c
            then subst c
            else unicodeChar c
    unicodeText     = mconcat . map unicodeChar . T.unpack

instance UnicodeSequence s => Encoded (PartialEncoding s) where
    encodingTag                     = PartialEncoding $ \ei _ -> eiTag ei
    replaceUnencodable subst' inner = PartialEncoding $ \ei subst -> 
        runPartialEncoding inner ei (\c -> runPartialEncoding (subst' c ) ei subst)
