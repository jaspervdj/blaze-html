> {-# LANGUAGE OverloadedStrings #-}
> import Prelude hiding (head)
> import Data.Monoid (mempty)
> import Html.Strict
> import Internal.HtmlMonad
> import Internal.EncodedHtml

> import Renderer.Utf8HtmlRenderer
> import Renderer.Latin1HtmlRenderer
> import Renderer.Ascii7HtmlRenderer

> import Criterion.Main
> import qualified Data.ByteString.Lazy as BL
> import Control.Monad (forM_)
> import qualified Data.Text as T

> testDoc :: Html h => h
> testDoc = concatenatedHtml $ do
>    html $ do head mempty
>              body $ text "Hello world. λf.(λx.fxx)(λx.fxx)"

Now run in `ghci`:

- `renderUtf8Html testDoc`
- `renderLatin1Html testDoc`
- `renderAscii7Html testDoc`

You will see that, for every renderer used, the output will be in the correct
format and will contain the correct encoding tag.

> main = defaultMain
>     [ bench "render/length" $ whnf (BL.length . renderTable) rows
>     ]
>   where
>     rows :: Int
>     rows = 1000
> 
>     renderTable :: Int -> BL.ByteString
>     renderTable n = renderUtf8Html $ doc n
> 
>     doc :: Html h => Int -> h
>     doc n = table $ concatenatedHtml $
>         forM_ [1..n] $ \row ->
>             tr $ forM_ (zip ['a'..'j'] [1..10]) $ \col ->
>                 td $ text $ T.pack (show col)
