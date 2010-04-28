-- | BigTable benchmark using the HTML package from hackage.
--
import Text.Html
import Criterion.Main

bigTable :: [[Int]] -> String
bigTable t = renderHtml $ table $ concatHtml $ map row t
  where
    row r = tr $ concatHtml $ map (td . stringToHtml . show) r

main = defaultMain
    [ bench "bigTable" $ nf bigTable myTable ]
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}
