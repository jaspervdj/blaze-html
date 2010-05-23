{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (ord)
import Data.Int (Int64)
import Data.Monoid (mconcat, mappend)

import qualified Data.Binary.Builder as B
import Criterion.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import qualified Text.Blaze.Internal.Utf8Builder as UB

main = defaultMain 
    [ bench "[String] -> Builder" $ whnf benchStrings strings
    , bench "[String] -> Builder'" $ whnf benchStrings' strings
    , bench "[ByteString] -> Builder" $ whnf benchByteStrings byteStrings
    , bench "[ByteString] -> Builder'" $ whnf benchByteStrings' byteStrings
    , bench "[Text] -> Builder" $ whnf benchText texts
    , bench "[Text] -> Builder'" $ whnf benchText' texts
    ]
  where
    strings :: [String]
    strings = replicate 100 "<img>" 
    {-# NOINLINE strings #-}

    byteStrings :: [S.ByteString]
    byteStrings = replicate 100 "<img>"
    {-# NOINLINE byteStrings #-}

    texts :: [Text]
    texts = replicate 100 "<img>"
    {-# NOINLINE texts #-}

benchStrings :: [String] -> Int64
benchStrings = BL.length . B.toLazyByteString . mconcat
             . concatMap (map $ B.singleton . fromIntegral . ord)

benchStrings' :: [String] -> Int64
benchStrings' = BL.length . UB.toLazyByteString
              . mconcat . map UB.fromString

benchByteStrings :: [S.ByteString] -> Int64
benchByteStrings = BL.length . B.toLazyByteString
                 . mconcat . map B.fromByteString

benchByteStrings' :: [S.ByteString] -> Int64
benchByteStrings' = BL.length . UB.toLazyByteString
                  . mconcat . map UB.unsafeFromByteString

benchText :: [Text] -> Int64
benchText = BL.length . B.toLazyByteString . mconcat
          . map (B.fromByteString . encodeUtf8)

benchText' :: [Text] -> Int64
benchText' = BL.length . UB.toLazyByteString . mconcat
           . map UB.fromText
