import Control.Concurrent (forkIO,killThread)
import Control.Monad (forever)
import Criterion.Main (defaultMain,bench,Benchmark,whnf)
import Data.Monoid (mappend)
import Network (PortID(PortNumber),Socket)
import qualified Network as N
import Text.BlazeHtml.Internal.Html
import Text.BlazeHtml.Text (Text)
import qualified Text.BlazeHtml.Text as T
import Text.BlazeHtml.Render.HtmlText
import System.IO
import System.Console.GetOpt
import System.Directory (getCurrentDirectory,removeFile)
import System.Environment

-- | See the results of --help for information on what each option means.
data Options = Options
 { options_appendScale :: Int
 , options_iobaseline  :: Bool
 } deriving Show

-- | Default command line options.
defaultOptions :: Options
defaultOptions = Options { options_appendScale = 100
                         , options_iobaseline  = False }

options :: [OptDescr (Options -> Options)]
options = [Option [] ["append-scale"]
          (ReqArg (\e opts -> opts {options_appendScale = read e }) "SCALE")
          "Number of times to append elements together."
          ,Option [] ["io-baseline"]
          (ReqArg (\e opts -> opts {options_iobaseline  = e=="yes"}) "YESNO")
          "Perform the IO baseline test."]

-- | Grab the options.
benchmarkOpts :: [String] -> (Options, [String])
benchmarkOpts argv =
    case getOpt Permute options argv of
      (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
      (_,_,er) -> error (concat er ++ usageInfo header options)
        where header = "Usage: benchmark [OPTION...]"

-- | Just give us the end result options; this parsing for us.
getOptions :: [String] -> Options
getOptions = fst . benchmarkOpts

-- | No configuration, straight run sockets followed by file tests.
main :: IO ()
main = do args <- getArgs
          let opts = getOptions args
          runBenchmarks opts

-- | Run benchmarks with options.
runBenchmarks :: Options -> IO ()
runBenchmarks opts = do
  if (options_iobaseline opts) then withIoBaselineTest else runTests
      where runTests = defaultMain $ pureTests

pureTests :: [Benchmark]
pureTests = [appendBench,nestingElements]

-- | Benchmark for appending two simple elements.
appendBench :: Benchmark
appendBench = bench "appendBench" $ flip whnf () $ \() ->
    renderHtmlText $ a `mappend` a
        where a = renderElement (T.pack "p") (renderUnescapedText simpleTestString)

-- | Benchmark for nesting elements.
nestingElements :: Benchmark
nestingElements = bench "insertElements" $ flip whnf () $ \() ->
    renderHtmlText $ applyntimes adopt str 20
        where adopt = renderElement (T.pack "div")
              str = renderUnescapedText simpleTestString

applyntimes :: (a -> a) -> a -> Int -> a
applyntimes f start = (iterate f start!!)

-- | Just a 1KB text string.
simpleTestString :: Text
simpleTestString = T.pack $ replicate (1024*1024*100 :: Int) 'a'

-- | Include the IO baseline tests with the rest.
withIoBaselineTest :: IO ()
withIoBaselineTest = do
  N.withSocketsDo   $
    withSocketTests $ \socketTests ->
    withFileTests   $ \fileTests   -> do
      defaultMain $ socketTests ++ fileTests ++ pureTests

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
fileIO h = [bench "file io test" $ fileTest h largeText]

-- | Straight writing the given data to the given handle.
fileTest :: Handle -> Text -> IO ()
fileTest h t = T.hPutStr h t

-- | A simple large set.
largeText :: Text
largeText =
    renderHtmlText $ renderElement "p" $
      renderUnescapedText $ T.pack $ replicate largeSize 'a'
          where largeSize = 1024 * 1024

-- | Initialise the socket tests.
withSocketTests :: ([Benchmark] -> IO ()) -> IO ()
withSocketTests m = do
  server <- N.listenOn portID
  sid <- forkIO $ runSocketServer server
  handle <- N.connectTo "127.0.0.1" portID
  m [bench "streamIO" $ streamIO handle largeText]
  N.sClose server
  killThread sid

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
