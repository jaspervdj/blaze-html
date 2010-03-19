{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.Render.HtmlIO
    ( HtmlIO
    , renderHtmlIO
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Text.BlazeHtml.Internal.Html

-- | This type explains the approach taken in 
--
--    http://github.com/jaspervdj/BlazeHtml/blob/master/src/Text/BlazeHtml.hs
--
-- as a special instance of the Html typeclass. The only difference is that
-- we require setting of attributes to explicitly mention the Html element that
-- should receive these attributes.
newtype HtmlIO = HtmlIO
    { runHtmlIO :: ReaderT (Text -> IO ()) (ReaderT Attributes IO) ()
    }

-- | Retrieve the outputer from the outer reader and lift it.
getOutputter = (liftIO .) `liftM` ask

-- | Retrieve the attributes from
getAttributes = lift ask

-- | Helper function to render the attributes.
renderAttributes :: Attributes -> Text
renderAttributes [] = T.empty
renderAttributes t  = foldr append mempty $ t
  where
  append (k, v) = mappend (mconcat [" ", k, "=\"", v, "\""])

instance Monoid HtmlIO where
  mempty        = HtmlIO $ return ()
  mappend m1 m2 = HtmlIO $ runHtmlIO m1 >> runHtmlIO m2

instance Html HtmlIO where
  renderUnescapedText t = HtmlIO $ do
    out <- getOutputter
    out t
  renderLeafElement t = HtmlIO $ do
    out <- getOutputter
    attrs <- getAttributes
    out $ "<" `mappend` t
    out $ renderAttributes attrs
    out $ "/>"
  modifyUnescapedAttributes f h = 
    HtmlIO $ ReaderT $ \out -> local f (runReaderT (runHtmlIO h) out)
  renderElement t h = HtmlIO $ do
    out <- getOutputter
    attrs <- getAttributes
    out $ "<" `mappend` t
    out $ renderAttributes attrs
    out $ ">"
    runHtmlIO $ modifyUnescapedAttributes (const []) h
    out $ "</" `mappend` t `mappend` ">"

-- | Output html to stdout.
renderHtmlIO :: (Text -> IO ()) -> HtmlIO -> IO ()
renderHtmlIO out = (`runReaderT` []) . (`runReaderT` out) . runHtmlIO
