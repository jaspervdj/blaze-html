{-# LANGUAGE OverloadedStrings #-}
-- | Two benchmarks using the regular HTML package.
module Main where
import Text.Html
import Criterion.Main
import Data.Char (ord, chr)
import Debug.Trace
import Data.List (foldl')
import Data.Bits ((.&.), shiftR)
import Data.Monoid
import Control.DeepSeq  (deepseq)
import Control.Parallel.Strategies
import Control.Exception (evaluate)
import Data.Binary.Builder (Builder, fromByteString, toLazyByteString, singleton)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Array (Array)
import qualified Data.Array as A

-- Needed for encoding strings easily. Subject to removal.
import Codec.Binary.UTF8.String (encode)

infixr 5 :#:
{-
April 14th, 2010 - Simon Meier - Current State
==============================================

All numbers were measured on an Intel(R) Core(TM)2 Duo CPU T7500  @ 2.20GHz
laptop with 2GB RAM.

The benchmarks below stem from

  http://code.google.com/p/spitfire/source/browse/#svn/trunk/tests/perf

Genshi tag builder                            480.73 ms
Genshi template                               325.74 ms
Genshi template + tag builder                 521.59 ms
Mako Template                                  41.66 ms
ClearSilver                                    60.91 ms
Djange template                               441.88 ms
Spitfire template                              37.06 ms
Spitfire template -O1                          19.93 ms
Spitfire template -O2                           7.74 ms
Spitfire template -O3                           7.76 ms
Spitfire template -O4                           5.53 ms
StringIO                                       59.50 ms
cStringIO                                      10.08 ms
list concat                                     6.39 ms

PHP: 6.95916175842 ms

Note that (as far as I can tell) none of the < 60ms engines is doing proper
HTML escaping. Here, it is not required, but we would like to free the template
designer from thinking about it. Our current implementations fare as follows:

html/bigTable    12.74035  ms
cps/bigTable      4.063946 ms 
strCopy/bigTable  1.137269 ms

html/basic        36.04742 us
cps/basic         16.75962 us
strCopy/basic     5.754241 us

Legend: 'html' is the std. Haskell HTML library. 'cps' is my CPS-based
implementation, and 'strCopy' is the time for copying the whole result string
(i.e. the best time we can hope for when using Haskell strings). We see that
the CPS implementation is in both cases at least two times faster and does
require spend two additional "operations" per character compared to just
copying the result string.

I think these numbers are very encouraging. Especially, as they do not yet
include the task of encoding the resulting string. Fusing rendering and
encoding using a Builder Monoid may again give us a nice speedup compared to
existing solutions.
-}

bigTable :: [[Int]] -> String
bigTable t = renderHtml $ table $ concatHtml $ map row t
  where
    row r = tr $ concatHtml $ map (td . stringToHtml . show) r

bigTable' :: [[Int]] -> String
bigTable' t = renderHtml $ table $
    foldl' (\h row -> h +++ (
        tr $ foldl' (\h' col -> h' +++ td
            (stringToHtml $ show col))
                noHtml row)) noHtml t

basic :: (String, String, [String]) -- ^ (Title, User, Items)
      -> String
basic (title', user, items) = renderHtml $ concatHtml
    [ header $ thetitle $ stringToHtml title'
    , body $ concatHtml
        [ thediv $ h1 ! [identifier "header"] $ stringToHtml title'
        , p $ stringToHtml $ "Hello, " ++ user ++ "!"
        , p $ stringToHtml $ "Hello, me!"
        , p $ stringToHtml $ "Hello, world!"
        , h2 $ stringToHtml "Loop"
        , concatHtml $ map (li . stringToHtml) items
        , thediv ! [identifier "footer"] $ stringToHtml ""
        ]
    ]

main = defaultMain
    [ bench "html/bigTable"   $ nf (B.length . B.pack . encode . bigTable) myTable
    , bench "render bigTable'" $ nf bigTable' myTable
    , bench "cps/bigTable"     $ nf (B.length . B.pack . encode . bigTableH) myTable
    , bench "builder/bigTable" $ nf (BL.length . bigTableHB) myTable
    , bench "strCopy/bigTable" $ nf (B.length . B.pack . encode . strCopy) bigTableString
    , bench "render bigTableHS" $ nf bigTableHS rows
    , bench "html/basic"       $ nf (B.length . B.pack . encode . basic) basicData
    , bench "cps/basic"        $ nf (B.length . B.pack . encode . basicH)  basicData
    , bench "builder/basic" $ nf (BL.length . basicHB) basicData
    , bench "strCopy/basic"    $ nf (B.length . B.pack . encode . strCopy) basicString
    , bench "render basicHS" $ nf basicHS basicData
    ]
    -- mapM_ (\r -> evaluate (deepseq (bigTable r) ())) (replicate 100 rows)
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}

    basicData :: (String, String, [String])
    basicData = ("Just a test", "joe", items)
    items :: [String]
    items = map (("Number " ++) . show) [1 .. 14]

    bigTableString = bigTableH myTable
    basicString    = basicH basicData
    strCopy = foldr (:) []
    {-# NOINLINE strCopy #-}


------------------------------------------------------------------------------
-- A continuation passing implementation for fast String append
------------------------------------------------------------------------------

-- Twice as fast as the existing 'html' library.
--
-- An interesting remaining option: Using strict strings. See below.

newtype H = H { runH :: (String -> String) -> (String -> String) }

rawStringH :: String -> H
rawStringH s = H $ \_ k -> s ++ k

stringH :: String -> H
stringH s = H $ \_ -> escape
  where
  escape k = go s
    where
    go []       = k
    go ('<':ss) = '&':'l':'t':';'         : go ss
    go ('>':ss) = '&':'g':'t':';'         : go ss
    go ('&':ss) = '&':'a':'m':'p':';'     : go ss
    go ('"':ss) = '&':'q':'u':'o':'t':';' : go ss
    go (c  :ss) = c                       : go ss

tagH :: String -> H -> H
tagH tag = \inner -> H $ \attrs k ->
  '<':tag ++ attrs ('>' : runH inner id (endTag ++ k))
  where
  endTag = '<':'/':tag ++ ">"

addAttrH :: String -> String -> H -> H
addAttrH key = \value h -> H $ \attrs k ->
  runH h (\a -> attrs (' ':(key ++ '=':'"':escape value a))) k
  where
  escape v a = go v
    where
    go []       = '"' : a
    go ('&':ss) = '&':'a':'m':'p':';'     : go ss
    go ('"':ss) = '&':'q':'u':'o':'t':';' : go ss
    go (c  :ss) = c                       : go ss

tableH = tagH "table"
trH    = tagH "tr"
tdH    = tagH "td"

renderH :: H -> String
renderH h = runH h id []

instance Monoid H where
  mempty          = H $ \_ k -> k
  {-# INLINE mempty #-}
  h1 `mappend` h2 = H $ \attrs k -> runH h1 attrs (runH h2 attrs k)
  {-# INLINE mappend #-}
  mconcat hs = H $ \attrs k -> foldr (\h k' -> runH h attrs k') k hs
  {-# INLINE mconcat #-}

bigTableH :: [[Int]] -> String
bigTableH t = renderH $ tableH $ mconcat $ map row t
  where
    row r = trH $ mconcat $ map (tdH . stringH . show) r

htmlH :: H -> H
htmlH inner = 
  -- a too long string for the fairness of comparison
  rawStringH "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tagH "html" inner
  
headerH = tagH "header"
titleH = tagH "title"
bodyH = tagH "body"
divH = tagH "div"
h1H = tagH "h1"
h2H = tagH "h2"
pH = tagH "p"
liH = tagH "li"

idAH = addAttrH "id"

basicH :: (String, String, [String]) -- ^ (Title, User, Items)
      -> String
basicH (title', user, items) = renderH $ htmlH $ mconcat
    [ headerH $ titleH $ stringH title'
    , bodyH $ mconcat
        [ divH $ idAH "header" (h1H $ stringH title')
        , pH $ stringH $ "Hello, " ++ user ++ "!"
        , pH $ stringH $ "Hello, me!"
        , pH $ stringH $ "Hello, world!"
        , h2H $ stringH "Loop"
        , mconcat $ map (liH . stringH) items
        , idAH "footer" (divH $ mempty)
        ]
    ]

------------------------------------------------------------------------------
-- Context-passing plus strict strings
------------------------------------------------------------------------------

-- OUCH: The current implementation is horribly slow!
--       I don't know why...but it was also just a blind try...

data SS = SNil | {-# UNPACK #-} !Char :#: {-# UNPACK #-} !SS
  deriving( Eq, Ord {-! NFData !-} )

instance NFData SS where
        -- due to the strictness of the constructor WHNF suffices
        rnf x = x `seq` ()

foldrSS :: (Char -> a -> a) -> a -> SS -> a
foldrSS fSCons fSNil = go
  where
  go SNil     = fSNil
  go (x:#:xs) = fSCons x (go xs)

toSS :: String -> SS
toSS = foldr (:#:) SNil

toSScont :: String -> SS -> SS
toSScont str = (\ss -> foldr (:#:) ss str)

fromSS :: SS -> String
fromSS = foldrSS (:) []

instance Monoid SS where
  mempty = SNil
  {-# INLINE mempty #-}
  mappend xs ys = foldrSS (:#:) ys xs
  {-# INLINE mappend #-}


newtype HS = HS { runHS :: (SS -> SS) -> (SS -> SS) }

rawStringHS :: String -> HS
rawStringHS str = HS $ \_ -> toSScont str

stringHS :: String -> HS
stringHS s = HS $ \_ -> escape
  where
  escape k = go s
    where
    go []       = k
    go ('<':ss) = '&':#:'l':#:'t':#:';'             :#: go ss
    go ('>':ss) = '&':#:'g':#:'t':#:';'             :#: go ss
    go ('&':ss) = '&':#:'a':#:'m':#:'p':#:';'       :#: go ss
    go ('"':ss) = '&':#:'q':#:'u':#:'o':#:'t':#:';' :#: go ss
    go (c  :ss) = c                                 :#: go ss

tagHS :: String -> HS -> HS
tagHS tag = \inner -> HS $ \attrs k ->
  '<':#:(ssTag `mappend` attrs ('>' :#: runHS inner id (endTag `mappend` k)))
  where
  ssTag  = toSS tag
  endTag = toSS $ '<':'/':tag ++ ">"

addAttrHS :: String -> String -> HS -> HS
addAttrHS key = \value h -> HS $ \attrs k ->
  runHS h (\a -> attrs (' ':#:(ssKey `mappend` ('=':#:'"':#:escape value a)))) k
  where
  ssKey = toSS key
  escape v a = go v
    where
    go []       = '"':#: a
    go ('&':ss) = '&':#:'a':#:'m':#:'p':#:';'       :#: go ss
    go ('"':ss) = '&':#:'q':#:'u':#:'o':#:'t':#:';' :#: go ss
    go (c  :ss) = c                                 :#: go ss

tableHS = tagHS "table"
trHS    = tagHS "tr"
tdHS    = tagHS "td"

renderHS :: HS -> SS
renderHS h = runHS h id SNil

instance Monoid HS where
  mempty          = HS $ \_ k -> k
  {-# INLINE mempty #-}
  h1 `mappend` h2 = HS $ \attrs k -> runHS h1 attrs (runHS h2 attrs k)
  {-# INLINE mappend #-}
  mconcat hs = HS $ \attrs k -> foldr (\h k' -> runHS h attrs k') k hs
  {-# INLINE mconcat #-}

bigTableHS :: Int -> SS
bigTableHS n = renderHS $ tableHS $ mconcat $ map row [1 .. n]
  where
    row x = trHS $ mconcat $ map (tdHS . stringHS . show) ([x .. x + 10])

htmlHS :: HS -> HS
htmlHS inner = 
  -- a too long string for the fairness of comparison
  rawStringHS "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tagHS "html" inner
  
headerHS = tagHS "header"
titleHS = tagHS "title"
bodyHS = tagHS "body"
divHS = tagHS "div"
h1HS = tagHS "h1"
h2HS = tagHS "h2"
pHS = tagHS "p"
liHS = tagHS "li"

idAHS = addAttrHS "id"

basicHS :: (String, String, [String]) -- ^ (Title, User, Items)
      -> SS
basicHS (title', user, items) = renderHS $ htmlHS $ mconcat
    [ headerHS $ titleHS $ stringHS title'
    , bodyHS $ mconcat
        [ divHS $ idAHS "header" (h1HS $ stringHS title')
        , pHS $ stringHS $ "Hello, " ++ user ++ "!"
        , pHS $ stringHS $ "Hello, me!"
        , pHS $ stringHS $ "Hello, world!"
        , h2HS $ stringHS "Loop"
        , mconcat $ map (liHS . stringHS) items
        , idAHS "footer" (divHS $ mempty)
        ]
    ]
-- TODO: Complete

------------------------------------------------------------------------------
-- An implementation using a Builder monoid and UTF8
------------------------------------------------------------------------------



-- SM: General Overview
-- --------------------
--
-- Hi Jasper,
--
-- I commented below what you could improve to make the Builder implementation
-- faster. Most of the things I found out by experimenting with them during the
-- two attempts on full speed I made. Try to do the same by making a hypothesis
-- why a certain change should slow down the execution, changing it, and
-- verifying your hypothesis. In general, I try to think how to implement the
-- functionality in C and then map it back to the existing Haskell constructs
-- and the compiler optimizations necessary to translate these constructs to
-- the same good code.
--
-- Have a go. I'm looking forward to the new performance figures.
--
--   Simon
--



newtype HB = HB { runHB :: Builder -> Builder }

-- | Builder lookup array for characters <= 0xFF.
lookupBuilder :: Array Char Builder
lookupBuilder = A.listArray (chr 0, chr 0xFF) $ map createBuilder [0 .. 0xFF]
  where
    createBuilder c = singleton $ fromIntegral c

charToBuilder :: Char -> Builder
charToBuilder c | c <= chr 0x7F = lookupBuilder A.! c
                | otherwise = char8ToBuilder' (ord c)
  where
    char8ToBuilder' x
        | x <= 0x07FF =
             let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
                 x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
             in singleton x1 `mappend` singleton x2
        | x <= 0xFFFF =
             let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
                 x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                 x3 = fromIntegral $ (x .&. 0x3F) + 0x80
             in singleton x1 `mappend` singleton x2 `mappend` singleton x3
        | otherwise =
             let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
                 x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
                 x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                 x4 = fromIntegral $ (x .&. 0x3F) + 0x80
             in singleton x1 `mappend` singleton x2 `mappend` singleton x3
                             `mappend` singleton x4

stringToBuilder :: String -> Builder
stringToBuilder = mconcat . map charToBuilder

string8ToBuilder :: String -> Builder
string8ToBuilder = mconcat . map char8ToBuilder

char8ToBuilder :: Char -> Builder
char8ToBuilder = singleton . fromIntegral . ord

rawStringHB :: String -> HB
rawStringHB s = HB $ \_ -> stringToBuilder s

-- | Escaped builder lookup array for characters <= 0xFF.
lookupEscapedBuilder :: Array Char Builder
lookupEscapedBuilder = A.listArray (chr 0, chr 0xFF) $
    map createEscapedBuilder [chr 0 .. chr 0xFF]
  where
    createEscapedBuilder '<' = string8ToBuilder "&lt;"
    createEscapedBuilder '>' = string8ToBuilder "&gt;"
    createEscapedBuilder '&' = string8ToBuilder "&amp;"
    createEscapedBuilder '"' = string8ToBuilder "&quot;"
    createEscapedBuilder c   = charToBuilder c

stringHB :: String -> HB
stringHB s = HB $ \_ -> escape s
  where
  escape = mconcat . map escape'
  escape' c | c <= chr 0xFF = lookupEscapedBuilder A.! c
            | otherwise     = charToBuilder c

tagHB :: String -> HB -> HB
tagHB tag inner = HB $ \attrs ->
    char8ToBuilder '<' `mappend` tag'
                       `mappend` attrs
                       `mappend` close'
                       `mappend` runHB inner mempty
                       `mappend` string8ToBuilder "</"
                       `mappend` tag'
                       `mappend` close'
  where
    tag' = stringToBuilder tag
    close' = char8ToBuilder '>'

addAttrHB :: String -> String -> HB -> HB
addAttrHB key value h = HB $ \attrs ->
    runHB h $ attrs `mappend` char8ToBuilder ' '
                    `mappend` stringToBuilder key
                    `mappend` string8ToBuilder "=\""
                    `mappend` escape value
                    `mappend` char8ToBuilder '"'
  where
  escape = mconcat . map escape'
  escape' '&' = string8ToBuilder "&amp;"
  escape' '"' = string8ToBuilder "&quot;"
  escape' c   = charToBuilder c

tableHB = tagHB "table"
trHB    = tagHB "tr"
tdHB    = tagHB "td"

renderHB :: HB -> BL.ByteString
renderHB h = toLazyByteString $ runHB h mempty

instance Monoid HB where
  mempty          = HB $ \_ -> mempty
  {-# INLINE mempty #-}
  h1 `mappend` h2 = HB $ \attrs -> runHB h1 attrs `mappend` runHB h2 attrs
  {-# INLINE mappend #-}
  mconcat hs = HB $ \attrs -> foldr (\h k -> runHB h attrs `mappend` k) mempty hs
  {-# INLINE mconcat #-}

bigTableHB :: [[Int]] -> BL.ByteString
bigTableHB t = renderHB $ tableHB $ mconcat $ map row t
  where
    row r = trHB $ mconcat $ map (tdHB . stringHB . show) r

htmlHB :: HB -> HB
htmlHB inner = 
  -- a too long string for the fairness of comparison
  rawStringHB "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 FINAL//EN\">\n<!--Rendered using the Haskell Html Library v0.2-->\n"
  `mappend` tagHB "html" inner
  
headerHB = tagHB "header"
titleHB = tagHB "title"
bodyHB = tagHB "body"
divHB = tagHB "div"
h1HB = tagHB "h1"
h2HB = tagHB "h2"
pHB = tagHB "p"
liHB = tagHB "li"

idAHB = addAttrHB "id"

basicHB :: (String, String, [String]) -- ^ (Title, User, Items)
      -> BL.ByteString
basicHB (title', user, items) = renderHB $ htmlHB $ mconcat
    [ headerHB $ titleHB $ stringHB title'
    , bodyHB $ mconcat
        [ divHB $ idAHB "header" (h1HB $ stringHB title')
        , pHB $ stringHB $ "Hello, " `mappend` user `mappend` "!"
        , pHB $ stringHB $ "Hello, me!"
        , pHB $ stringHB $ "Hello, world!"
        , h2HB $ stringHB "Loop"
        , mconcat $ map (liHB . stringHB) items
        , idAHB "footer" (divHB $ mempty)
        ]
    ]
