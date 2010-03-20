{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main (defaultMain,bench)

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Text()
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network as N
import Network (PortID(PortNumber),Socket)
import System.IO (Handle)

main :: IO ()
main = N.withSocketsDo $ do
         server <- N.listenOn portID
         forkIO $ runSocketServer server
         handle <- N.connectTo "127.0.0.1" portID
         let contents = T.pack $ replicate 100000 'a'
         defaultMain [bench "fileIO" fileIO
                     ,bench "streamIO" $ streamIO handle contents]

fileIO :: IO ()
fileIO = return ()

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
