module Text.BlazeHtml.Renderers
    ( Renderer
    , stdoutRenderer
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO

type Renderer = Text -> IO ()

stdoutRenderer :: Renderer
stdoutRenderer = TextIO.putStr
