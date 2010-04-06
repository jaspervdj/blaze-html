> {-# LANGUAGE OverloadedStrings #-}
> import Html.Strict
> import Internal.HtmlMonad
> import Internal.EncodedHtml

> import Renderer.Utf8HtmlRenderer
> import Renderer.Latin1HtmlRenderer
> import Renderer.Ascii7HtmlRenderer

> testDoc :: Html h => h
> testDoc = concatenatedHtml $ do
>    html $ text "Hello world."

Now run in `ghci`:

- `renderUtf8Html testDoc`
- `renderLatin1Html testDoc`
- `renderAscii7Html testDoc`
