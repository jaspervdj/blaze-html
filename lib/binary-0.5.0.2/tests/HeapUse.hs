-- Checks heap behavior of getBytes

import Data.Binary.Get (runGet, getBytes)

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as L

main = do
       let x = (L.take 110000042 $ L.iterate (+1) 0)
       mapM_ (print . L.length) (chunks 20000000 x)

chunks n = runGet (unfoldM f)
  where f = do x <- getBytes 20000000 
               return $ if L.null x then Nothing else Just x

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = f >>= maybe (return []) (\x -> liftM (x:) (unfoldM f))
