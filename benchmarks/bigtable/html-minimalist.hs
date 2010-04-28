-- | BigTable benchmark using the html-minimalist package from hackage.
--
import Text.HTML.Light hiding (map)
import Criterion.Main

bigTable :: [[Int]] -> String
bigTable t =
    renderXHTML xhtml_1_0_strict $ html [] $ return $ table [] $ map row t
  where
    row r = tr [] $ map (td [] . return . cdata . show) r

main = defaultMain
    [ bench "bigTable" $ nf bigTable myTable ]
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}
