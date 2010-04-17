-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Parallel
-- Copyright   :  (c) Don Stewart 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Exception, Control.Concurrent)
--
-- A parallel batch driver for running QuickCheck on threaded or SMP systems.
-- See the /Example.hs/ file for a complete overview.
--

module Parallel (
    pRun,
    pDet,
    pNon
  ) where

import Test.QuickCheck
import Data.List
import Control.Concurrent
import Control.Exception  hiding (evaluate)
import System.Random
import System.IO          (hFlush,stdout)
import Text.Printf

type Name   = String
type Depth  = Int
type Test   = (Name, Depth -> IO String)

-- | Run a list of QuickCheck properties in parallel chunks, using
-- 'n' Haskell threads (first argument), and test to a depth of 'd'
-- (second argument). Compile your application with '-threaded' and run
-- with the SMP runtime's '-N4' (or however many OS threads you want to
-- donate), for best results.
--
-- > import Test.QuickCheck.Parallel
-- >
-- > do n <- getArgs >>= readIO . head
-- >    pRun n 1000 [ ("sort1", pDet prop_sort1) ]
--
-- Will run 'n' threads over the property list, to depth 1000.
--
pRun :: Int -> Int -> [Test] -> IO ()
pRun n depth tests = do
    chan <- newChan
    ps   <- getChanContents chan
    work <- newMVar tests

    forM_ [1..n] $ forkIO . thread work chan

    let wait xs i
            | i >= n     = return () -- done
            | otherwise = case xs of
                    Nothing : xs -> wait xs $! i+1
                    Just s  : xs -> putStr s >> hFlush stdout >> wait xs i
    wait ps 0

  where
    thread :: MVar [Test] -> Chan (Maybe String) -> Int -> IO ()
    thread work chan me = loop
      where
        loop = do
            job <- modifyMVar work $ \jobs -> return $ case jobs of
                        []     -> ([], Nothing)
                        (j:js) -> (js, Just j)
            case job of
                Nothing          -> writeChan chan Nothing -- done
                Just (name,prop) -> do
                    v <- prop depth
                    writeChan chan . Just $ printf "%d: %-25s: %s" me name v
                    loop


-- | Wrap a property, and run it on a deterministic set of data
pDet :: Testable a => a -> Int -> IO String
pDet a n = mycheck Det defaultConfig
    { configMaxTest = n
    , configEvery   = \n args -> unlines args } a

-- | Wrap a property, and run it on a non-deterministic set of data
pNon :: Testable a => a -> Int -> IO String
pNon a n = mycheck NonDet defaultConfig
    { configMaxTest = n
    , configEvery   = \n args -> unlines args } a

data Mode = Det | NonDet

------------------------------------------------------------------------

mycheck :: Testable a => Mode -> Config -> a -> IO String
mycheck Det config a = do
     let rnd = mkStdGen 99  -- deterministic
     mytests config (evaluate a) rnd 0 0 []

mycheck NonDet config a = do
    rnd <- newStdGen        -- different each run
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO String
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise = do
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             return ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO String
done mesg ntest stamps =
    return ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest
                          ++ " "
                          ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

forM_ = flip mapM_
