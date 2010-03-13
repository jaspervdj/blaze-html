# BlazeHtml

BlazeHtml is a fast HTML combinator library, currently in an experimental state.
The interface and modules are provisional and in development.

    import Data.Text (pack, unpack)

    import Text.BlazeHtml
    import Text.BlazeHtml.CommonElements

    main = do
        putStrLn $ unpack $ printHtml html
      where
        html = h1 (pack "BlazeHtml")
             . setAttribute (pack "class") (pack "notice") . div_ (
                     p (text $ pack "BlazeHtml is a HTML combinator library.")
                   . img (pack "logo.png") (pack "BlazeHtml logo.")
                   . p (text $ pack "This is a usage example.")
             )
