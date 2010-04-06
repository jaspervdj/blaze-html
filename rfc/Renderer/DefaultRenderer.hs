{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Renderer.DefaultRenderer
    ( DefaultRenderer
    , renderDefault
    ) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)

import Internal.UnicodeSequence
import Internal.EncodedHtml

-- | represent a Html document as a single line without line-breaks.
newtype DefaultRenderer s = DefaultRenderer
    { runDefaultRenderer :: s -> s
    } deriving (UnicodeSequence)

instance Monoid m => Monoid (DefaultRenderer m) where
    mempty = DefaultRenderer $ const mempty
    (DefaultRenderer m1) `mappend` (DefaultRenderer m2) =
        DefaultRenderer $ m1 `mappend` m2
    mconcat hs = DefaultRenderer $ \attrs ->
        mconcat $ map (`runDefaultRenderer` attrs) hs

instance Encoded s => Encoded (DefaultRenderer s) where
    encodingTag = DefaultRenderer $ const encodingTag
    replaceUnencodable subst inner = DefaultRenderer $ \attrs -> 
        replaceUnencodable ((`runDefaultRenderer` attrs) . subst)
                           (runDefaultRenderer inner attrs)

instance (Encoded s, UnicodeSequence s) => Html (DefaultRenderer s) where
    h1 `separate` h2 = DefaultRenderer $ \attrs -> mconcat
        [ runDefaultRenderer h1 attrs 
        , unicodeChar ' '
        , runDefaultRenderer h2 attrs
        ]
    leafElement tag = DefaultRenderer $ \attrs -> mconcat
        [ unicodeChar '<'
        , runDefaultRenderer tag mempty
        , attrs
        , unicodeString "/>"
        ]
    nodeElement tag inner = DefaultRenderer $ \attrs -> mconcat
        [ unicodeChar '<'
        , runDefaultRenderer tag mempty
        , attrs
        , unicodeChar '>'
        , runDefaultRenderer inner mempty
        , unicodeString "</"
        , runDefaultRenderer tag mempty
        , unicodeChar '>'
        ]
    addAttribute key value h = DefaultRenderer $ \attrs -> 
        runDefaultRenderer h $ mconcat
            [ unicodeChar ' '
            , runDefaultRenderer key mempty
            , unicodeChar '='
            , unicodeChar '"'
            , runDefaultRenderer value mempty
            , unicodeChar '"'
            ] `mappend` attrs

renderDefault :: (Encoded s, UnicodeSequence s) => DefaultRenderer s -> s
renderDefault renderer = runDefaultRenderer renderer mempty
