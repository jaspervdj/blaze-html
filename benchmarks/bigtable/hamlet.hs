-- | BigTable benchmark implemented using Hamlet.
--
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Criterion.Main
import Text.Hamlet
import Text.Hamlet.Monad
import Numeric (showInt)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)

main = defaultMain
    [ bench "bigTable" $ nf bigTable bigTableData
    ]
  where
    rows :: Int
    rows = 1000

    bigTableData :: [[Int]]
    bigTableData = replicate rows [1..10]
    {-# NOINLINE bigTableData #-}

bigTable rows = fromJust $ hamletToText undefined [$hamlet|
%table
    $forall rows row
        %tr
            $forall row cell
                %td $showInt'.cell$
|]
  where
    showInt' i = Encoded $ T.pack $ showInt i ""
