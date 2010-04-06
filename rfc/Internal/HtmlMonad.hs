module Internal.HtmlMonad 
    ( HtmlMonad
    , concatenatedHtml
    , separatedHtml
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import Internal.EncodedHtml
import Internal.UnicodeSequence

data StrictPair h a = StrictPair {-# UNPACK #-} !h a

-- | Construct a pair with only one element filled in. The second element is
-- bottom, thus you should make sure the second element is never evaluated.
constructBottomPair :: h -> StrictPair h a
constructBottomPair = flip StrictPair $ error "_|_"

-- | Html Monad. This monad is abstracted over the concrete type of appending to
-- use with `>>`. We provide concrete instances as well.
newtype HtmlMonad h a = HtmlMonad 
    { runHtmlMonad :: (h -> h -> h) -> StrictPair h a
    }

instance Monoid h => Monoid (HtmlMonad h a) where
    mempty = HtmlMonad $ const $ constructBottomPair mempty
    mappend (HtmlMonad f1) (HtmlMonad f2) = HtmlMonad $ \appender ->
        let StrictPair h1 _ = f1 appender
            StrictPair h2 _ = f2 appender
        in constructBottomPair (h1 `mappend` h2)

instance UnicodeSequence h => UnicodeSequence (HtmlMonad h a) where
    unicodeChar = HtmlMonad . const . constructBottomPair . unicodeChar
    unicodeText = HtmlMonad . const . constructBottomPair . unicodeText

instance Encoded h => Encoded (HtmlMonad h a) where
    encodingTag = HtmlMonad $ const $ constructBottomPair encodingTag
    replaceUnencodable subst (HtmlMonad f) = HtmlMonad $ \appender ->
        let StrictPair h a = f appender
            subst' c = let HtmlMonad f'    = subst c
                           StrictPair h' _ = f' appender
                       in h'
        in StrictPair (replaceUnencodable subst' h) a

instance Html h => Html (HtmlMonad h a) where
    separate (HtmlMonad f1) (HtmlMonad f2) = HtmlMonad $ \appender ->
        let StrictPair h1 _ = f1 appender
            StrictPair h2 a = f2 appender
        in StrictPair (h1 `separate` h2) a
    leafElement (HtmlMonad f) = HtmlMonad $ \appender ->
        let StrictPair h a = f appender
        in StrictPair (leafElement h) a
    nodeElement (HtmlMonad f1) (HtmlMonad f2) = HtmlMonad $ \appender ->
        let StrictPair h1 a = f1 appender
            StrictPair h2 _ = f2 appender
        in StrictPair (nodeElement h1 h2) a
    addAttribute (HtmlMonad k) (HtmlMonad v) (HtmlMonad h) = HtmlMonad $ \app ->
        let StrictPair kv _ = k app
            StrictPair vv _ = v app
            StrictPair hv a = h app
        in StrictPair (addAttribute kv vv hv) a
    
-- Note that the monad laws hold true for the HTML monad, meaning:
--
--     return a >>= f  = f a
--     m >>= return    = m
--     (m >>= f) >>= g = m >>= (\x -> f x >>= g)
instance Monoid h => Monad (HtmlMonad h) where
    return a = HtmlMonad $ const $ StrictPair mempty a
    (HtmlMonad f1) >> (HtmlMonad f2) = HtmlMonad $ \appender ->
        let StrictPair h1 _ = f1 appender
            StrictPair h2 a = f2 appender
        in StrictPair (h1 `appender` h2) a
    (HtmlMonad f1) >>= f = HtmlMonad $ \appender ->
        let StrictPair h1 a = f1 appender
            StrictPair h2 b = runHtmlMonad (f a) appender
        in StrictPair (h1 `appender` h2) b

concatenatedHtml :: Html h => HtmlMonad h a -> h
concatenatedHtml m = let StrictPair h _ = ($ mappend) $ runHtmlMonad m
                     in h

separatedHtml :: Html h => HtmlMonad h a -> h
separatedHtml m = let StrictPair h _ = ($ separate) $ runHtmlMonad m
                  in h
