import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Criterion.Main (defaultMain,bench,Benchmark)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network (PortID(PortNumber),Socket)
import qualified Network as N
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlText
import System.IO

main :: IO ()
main = N.withSocketsDo $ do
         server <- N.listenOn portID
         forkIO $ runSocketServer server
         handle <- N.connectTo "127.0.0.1" portID
         fh <- openFile "./testfile.txt" WriteMode
         let contents = T.pack $ replicate 100000 'a'
         defaultMain $ (fileIO fh) ++ [bench "streamIO" $ streamIO handle contents]
         hClose fh

fileIO :: Handle -> [Benchmark]
fileIO h = [bench "file io test" $ fileTest h simpleText]

fileTest :: Handle -> Text -> IO ()
fileTest h t = T.hPutStr h t

simpleText :: Text
simpleText = renderHtmlText (addUnescapedAttributes [("class","demo")] $
                                                    renderLeafElement "p")

-- | Run a simple fileIO test.
streamIO :: Handle -> Text -> IO ()
streamIO h t = do
  T.hPutStr h t
  return ()

-- | Run a socket server to connect to for testing socket sending.
runSocketServer :: Socket -> IO ()
runSocketServer server = do
  (h,_,_) <- N.accept server
  forever $ T.hGetContents h

-- | The hosting/connect to port number for socket testing.
portID :: PortID
portID = PortNumber $ fromIntegral (11111 :: Int)
