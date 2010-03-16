{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml
    ( Attributes
    , Html
    , renderHtml
    , renderLeafElement
    , renderElement
    , renderText
    , setAttributes
    , (!)
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as M

import Text.BlazeHtml.Renderers

type Attributes = Map Text Text

type Html = ReaderT Renderer (StateT Attributes IO)

renderHtml :: Renderer -> Html () -> IO ()
renderHtml renderer html = evalStateT (runReaderT html renderer) M.empty

renderAttributes :: Attributes -> Text
renderAttributes = M.foldWithKey append T.empty
  where
    append k v = T.append $ " " `T.append` k `T.append` "=\""
                                `T.append` v `T.append` "\""

renderLeafElement :: Text -> Html ()
renderLeafElement tag = do 
    renderer <- ask
    attributes <- get
    liftIO $ renderer $ "<" `T.append` tag
                            `T.append` renderAttributes attributes
                            `T.append` " />"
    put M.empty

renderElement :: Text -> Html () -> Html ()
renderElement tag inner = do
    renderer <- ask
    attributes <- get
    liftIO $ renderer $ "<" `T.append` tag
                            `T.append` renderAttributes attributes
                            `T.append` ">"
    put M.empty
    inner
    liftIO $ renderer $ "</" `T.append` tag `T.append` ">"

renderText :: Text -> Html ()
renderText text = do
    renderer <- ask
    liftIO $ renderer text

setAttributes :: [(Text, Text)] -> Html ()
setAttributes attributes = do
    attributes' <- get
    put $ M.fromList attributes `M.union` attributes'

(!) :: Html () -> [(Text, Text)] -> Html ()
html ! attributes = setAttributes attributes >> html
