-- | Two benchmarks using the regular HTML package.
import Text.Html
import Criterion.Main
import Debug.Trace
import Data.List (foldl')
import Data.Monoid
import Control.DeepSeq  (deepseq)
import Control.Exception (evaluate)

bigTable :: Int -> String
bigTable n = renderHtml $ table $ concatHtml $ map row [1 .. n]
  where
    row x = tr $ concatHtml $ map (td . stringToHtml . show) (zip ['a' .. 'j'] [x .. x + 10])

bigTable' :: Int -> String
bigTable' n = renderHtml $ table $
    foldl' (\h row -> h +++ (
        tr $ foldl' (\h' col -> h' +++ td
            (stringToHtml $ show col))
                noHtml (zip ['a'..'j'] [row .. row + 10]))) noHtml [1..n]

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

main = 
    defaultMain
    [ -- bench "render bigTable" $ nf bigTable rows
    -- , bench "render bigTable'" $ nf bigTable' rows
    -- , bench "render bigTableH" $ nf bigTableH rows
      bench "render basic" $ nf basic basicData
    , bench "render basicH" $ nf basicH basicData
    ]
    -- mapM_ (\r -> evaluate (deepseq (bigTable r) ())) (replicate 100 rows)
  where
    rows :: Int
    rows = 1000

    basicData :: (String, String, [String])
    basicData = ("Just a test", "joe", items)
    items :: [String]
    items = map (("Number " ++) . show) [1 .. 14]


------------------------------------------------------------------------------
-- A Context passing implementation
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

bigTableH :: Int -> String
bigTableH n = renderH $ tableH $ mconcat $ map row [1 .. n]
  where
    row x = trH $ mconcat $ map (tdH . stringH . show) (zip ['a' .. 'j'] [x .. x + 10])

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

data SS = SEmpty | SCons {-# UNPACK #-} !Char {-# UNPACK #-} !SS
  deriving( Eq, Ord )

toSS :: String -> SS
toSS = foldr SCons SEmpty

instance Monoid SS where
  mempty = SEmpty
  {-# INLINE mempty #-}
  mappend xs ys = go xs
    where
    go SEmpty       = ys
    go (SCons x xs) = SCons x (go xs)


-- TODO: Complete
