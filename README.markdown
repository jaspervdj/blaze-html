# BlazeHtml

BlazeHtml is a fast HTML combinator library, currently in an experimental state.
The interface and modules are provisional and in development. Currently, the
plan is to start serious development on this library during ZuriHac.

    {-# LANGUAGE OverloadedStrings #-}

    import Text.BlazeHtml
    import Text.BlazeHtml.HtmlElements
    import Text.BlazeHtml.Renderers

    main = renderHtml stdoutRenderer $ do
        h1 "BlazeHtml" ! [("id", "header")]
        img "logo.png" "BlazeHtml logo"

        setAttributes [("class", "intro")]
        p $ do text "BlazeHtml is a blazing fast HTML combinator library."
               em $ text "BlazeHtml uses a monadic interface."
               text "This gives us very readable code."

# Conventions

The idea is to follow a common [style guide][].

[style guide]: http://github.com/tibbe/haskell-style-guide "Haskell Style Guide"
