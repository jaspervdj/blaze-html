module Main where

import Control.Concurrent (forkIO,killThread)
import Control.Monad (forever)
import Data.List
import Data.Monoid (mappend)
import Network (PortID(PortNumber),Socket)
import qualified Network as N
import System.IO
import System.Console.GetOpt
import System.Directory (getCurrentDirectory,removeFile)
import System.Environment

import Criterion.Main (defaultMain,bench,Benchmark)

import Text.Html ((<<),(+++))
import qualified Text.Html as Html

import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Render.HtmlText

-- | See the results of --help for information on what each option means.
data Options = Options
 { options_appendScale :: Int
 , options_iobaseline  :: Bool
 } deriving Show

-- | No configuration, straight run sockets followed by file tests.
main :: IO ()
main = do args <- getArgs
          let opts = getOptions args
          runBenchmarks opts

-- | Default command line options.
defaultOptions :: Options
defaultOptions = Options { options_appendScale = 100
                         , options_iobaseline  = False }

-- | Options menu-type-thing.
options :: [OptDescr (Options -> Options)]
options = [Option [] ["append-scale"]
          (ReqArg (\e opts -> opts {options_appendScale = read e }) "SCALE")
          "Number of times to append elements together."
          ,Option [] ["io-baseline"]
          (ReqArg (\e opts -> opts {options_iobaseline  = e=="yes"}) "YESNO")
          "Perform the IO baseline test."]

-- | Grab the options. This is yet to work properly (i.e. as I expect.).
--   Just edit the code to change the options for now.
benchmarkOpts :: [String] -> (Options, [String])
benchmarkOpts argv =
    case getOpt Permute options argv of
      (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
      (_,_,er) -> error (concat er ++ usageInfo header options)
        where header = "Usage: benchmark [OPTION...]"

-- | Just give us the end result options; this parsing for us.
getOptions :: [String] -> Options
getOptions = fst . benchmarkOpts

-- | Benchmark for appending two simple elements.
appendBench :: Text
appendBench = 
    renderHtmlText $ applyntimes (a `mappend`) a 3
        where a = renderElement (T.pack "p") (renderUnescapedText string)
              string = T.pack simpleTestString

-- | Benchmark for appending two simple elements using the old library.
appendBenchOld :: String
appendBenchOld =
   Html.renderHtml $ applyntimes (a +++) a 3
       where a = Html.p << simpleTestString

-- | Benchmark for nesting elements.
nestingElements :: Text
nestingElements =
    renderHtmlText $ applyntimes adopt str 3
        where adopt = renderElement (T.pack "div")
              str = renderUnescapedText string
              string = T.pack simpleTestString

-- | Benchmark for nesting elements using the old library.
nestingElementsOld :: String
nestingElementsOld =
   Html.renderHtml $ applyntimes adopt str 3
        where adopt = Html.thediv
              str = Html.toHtml simpleTestString

-- | Utility function to apply a function to a value n times.
applyntimes :: (a -> a) -> a -> Int -> a
applyntimes f start = (iterate f start!!)

-- | Just a 1KB text string.
simpleTestString :: String
simpleTestString = replicate (1024*1024 :: Int) 'a'

-- | Include the IO baseline tests with the rest.
runBenchmarks :: Options -> IO ()
runBenchmarks opts = do
  N.withSocketsDo   $
    withSocketTests $ \socketTests ->
    withFileTests   $ \fileTests   -> do
      defaultMain $ socketTests ++ fileTests

-- | With a set of file writing tests, perform IO action.
withFileTests :: ([Benchmark] -> IO ()) -> IO ()
withFileTests m = do
  dir <- getCurrentDirectory
  (n,fh) <- openTempFile dir "testfile"
  m $ fileIO fh
  hClose fh
  removeFile n

-- | Simple writing to file benchmarks.
fileIO :: Handle -> [Benchmark]
fileIO h = map (bench "fileIO appendBench" . fileTest h) benches where
    benches = [appendBench,nestingElements]

-- | Straight writing the given data to the given handle.
fileTest :: Handle -> Text -> IO ()
fileTest h t = T.hPutStr h t

-- | Initialise the socket tests.
withSocketTests :: ([Benchmark] -> IO ()) -> IO ()
withSocketTests m = do
  server <- N.listenOn portID
  sid <- forkIO $ runSocketServer server
  handle <- N.connectTo "127.0.0.1" portID
  m . map (bench "streamIO appendBench" . streamIO handle) $ sources
  N.sClose server
  killThread sid
      where sources = [appendBench,nestingElements]

-- | Run a simple fileIO test.
streamIO :: Handle -> Text -> IO ()
streamIO h t = do
  T.hPutStr h t
  return ()

-- | Run a socket server to connect to for testing socket sending.
runSocketServer :: Socket -> IO ()
runSocketServer server =
    forever $ do (h,_,_) <- N.accept server
                 T.hGetContents h

-- | The hosting/connect to port number for socket testing.
portID :: PortID
portID = PortNumber $ fromIntegral (11111 :: Int)
