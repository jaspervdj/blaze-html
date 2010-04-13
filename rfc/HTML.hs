-- | Two benchmarks using the regular HTML package.
import Text.Html
import Criterion.Main
import Debug.Trace
import Data.List (foldl')

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
basic (title', user, items) = renderHtml $ thehtml $ concatHtml
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
    [ bench "render bigTable" $ nf bigTable rows
    , bench "render bigTable'" $ nf bigTable' rows
    , bench "render basic" $ nf basic basicData
    ]
  where
    rows :: Int
    rows = 1000

    basicData :: (String, String, [String])
    basicData = ("Just a test", "joe", items)
    items :: [String]
    items = map (("Number " ++) . show) [1 .. 14]
