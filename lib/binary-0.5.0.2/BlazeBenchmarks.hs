{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary.Builder
import Criterion.Main
import Data.Char (ord)
import Data.Monoid (mconcat, mappend)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

main = defaultMain 
    [ bench "[String] -> Builder" $ whnf benchStrings strings
    , bench "[ByteString] -> Builder" $ whnf benchByteStrings byteStrings
    , bench "[ByteString] -> Builder'" $ whnf benchByteStrings' byteStrings
    , bench "[Text] -> Builder" $ whnf benchText texts
    ]
  where
    strings :: [String]
    strings = replicate 100 "<img>" 
    {-# NOINLINE strings #-}

    byteStrings :: [S.ByteString]
    byteStrings = replicate 100 "<img>"
    {-# NOINLINE byteStrings #-}

    texts :: [Text]
    texts = replicate 30 $
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec \
        \nunc purus, bibendum quis dapibus in, fermentum id ipsum. Morbi \
        \scelerisque accumsan sem, nec tempus tortor dictum eu. Etiam id \
        \elit quam, ac suscipit libero. Aenean eget interdum ligula. Donec \
        \id lorem eget sem iaculis rutrum vel ut libero. Quisque iaculis, \
        \sem quis rhoncus commodo, ante erat placerat dui, non accumsan \
        \nunc odio at est. In ac leo felis.  Quisque et arcu nec ligula \
        \dignissim scelerisque. Aliquam tincidunt lacus quis augue iaculis \
        \sed tincidunt nunc porttitor. Donec id nunc orci. Nunc quam erat, \
        \euismod id vestibulum vel, porttitor auctor erat. Vestibulum sed \
        \placerat augue. Morbi suscipit eleifend mauris ac gravida. Vivamus \
        \non vehicula augue. Pellentesque nec aliquet enim."
    {-# NOINLINE texts #-}

benchStrings :: [String] -> Int64
benchStrings = BL.length . toLazyByteString . mconcat
             . concatMap (map $ singleton . fromIntegral . ord)

benchByteStrings :: [S.ByteString] -> Int64
benchByteStrings = BL.length . toLazyByteString
                 . mconcat . map fromByteString

benchByteStrings' :: [S.ByteString] -> Int64
benchByteStrings' = BL.length . toLazyByteString
                  . mconcat . map fromByteString'

benchText :: [Text] -> Int64
benchText = BL.length . toLazyByteString . mconcat
          . map (fromByteString . encodeUtf8)
