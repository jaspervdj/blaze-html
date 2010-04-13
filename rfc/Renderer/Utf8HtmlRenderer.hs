{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Renderer.Utf8HtmlRenderer where

import qualified Data.ByteString.Lazy as BL

import Renderer.DefaultRenderer
import Builder.Utf8Builder
import Encoding.TotalEncoding
import Internal.Html

import Data.Monoid (Monoid (..))

newtype Utf8Html = Utf8Html
    { runUtf8Html :: Utf8Builder -> Utf8Builder
    } deriving (UnicodeSequence)

instance Monoid Utf8Html where
    mempty = Utf8Html $ const mempty
    (Utf8Html m1) `mappend` (Utf8Html m2) =
        Utf8Html $ m1 `mappend` m2
    mconcat ms = Utf8Html $ \attrs ->
        mconcat $ map (`runUtf8Html` attrs) ms

-- This is of course an instance of @Encoded@.
instance Encoded Utf8Html where
    encodingTag        = Utf8Html $ const $ ascii7String "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>"
    replaceUnencodable = const id

instance Html Utf8Html where
    h1 `separate` h2 = Utf8Html $ \attrs -> mconcat
        [ runUtf8Html h1 attrs 
        , ascii7Char ' '
        , runUtf8Html h2 attrs
        ]
    leafElement tag = Utf8Html $ \attrs -> mconcat
        [ ascii7Char '<'
        , runUtf8Html tag mempty
        , attrs
        , ascii7Char '/', ascii7Char '>'
        ]
    nodeElement tag inner = Utf8Html $ \attrs -> mconcat
        [ ascii7Char '<'
        , runUtf8Html tag mempty
        , attrs
        , ascii7Char '>'
        , runUtf8Html inner mempty
        , ascii7Char '<', ascii7Char '/'
        , runUtf8Html tag mempty
        , ascii7Char '>'
        ]
    addAttribute key value h = Utf8Html $ \attrs -> 
        runUtf8Html h $ mconcat
            [ ascii7Char ' '
            , runUtf8Html key mempty
            , ascii7Char '='
            , ascii7Char '"'
            , runUtf8Html value mempty
            , ascii7Char '"'
            ] `mappend` attrs

renderHtmlUtf8 :: Utf8Html -> BL.ByteString
renderHtmlUtf8 (Utf8Html r) = toLazyByteStringUtf8 $ r mempty
