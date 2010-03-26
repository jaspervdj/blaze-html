{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, head, span, putStr)
import qualified Prelude as P
import Text.BlazeHtml.Html as H
import Text.BlazeHtml.Attributes as A
import Text.BlazeHtml.Render.HtmlByteString
import Text.BlazeHtml.Render.HtmlPrettyText
import Text.BlazeHtml.Render.HtmlText
import Text.BlazeHtml.Render.Indent
import Text.BlazeHtml.Text (Text)
import Data.ByteString.Lazy (putStr)
import Data.Monoid
import BlazeHtmlContentData (dataAPI)
import Criterion
import Criterion.Main
import System.Console.GetOpt

infix 1 </

(</) :: Monoid h => (h -> h) -> [h] -> h
nest </ inner = nest (mconcat inner)

exampleContent :: (Html h) => h
exampleContent = html ! A.xmlns "http://www.w3.org/1999/xhtml" </ 
            [ head </ 
                [ H.title $ unescapedText "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
                , H.link ! [ A.type_ "text/css", A.rel "stylesheet", ("href", "http://wowprogramming.com/css/blueprint/reset.css")
                          , ("media", "screen, projection")]
                , H.link ! [ A.type_ "text/css", A.rel "stylesheet", ("href", "http://wowprogramming.com/css/blueprint/typography.css")
                          , ("media", "screen, projection")]
                , H.link ! [ A.type_ "text/css", A.rel "stylesheet", ("href", "http://wowprogramming.com/css/blueprint/print.css")
                          , ("media", "print")]
                , H.link ! [ A.type_ "text/css", A.rel "stylesheet", ("href", "http://wowprogramming.com/css/wowprogramming.css")
                          , ("media", "screen, projection")]
                , H.link ! [ A.rel "shortcut icon", ("href", "http://wowprogramming.com/images/favicon.ico")]
                , H.link ! [ A.rel "alternate", A.type_ "application/rss+xml"
                          , A.title "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
                          , ("href", "http://wowprogramming.com/history.rss")]
                , H.link ! [ A.rel "alternate", A.type_ "application/rss+xml"
                          , A.title "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons: docs/api"
                          , ("href", "http://wowprogramming.com/docs/api.rss")]
                ]

                , body </
                    [ div ! A.id "wrapper" </
                        [ div ! A.id "header" </
                            [ div ! A.id "search" $ 
                                form ! [A.id "search_form", ("action", "http://google.com/search")] </
                                    [ input ! [A.id "search_box", A.type_ "text", ("name", "q"), ("value", "Search using Google")]
                                    , input ! [A.type_ "hidden", ("name", "as_sitesearch"), ("value", "wowprogramming.com")]
                                    , input ! [A.id "search_button", A.type_ "submit", ("value", "")]]
                            , div ! A.id "login_status" </
                                [ a ! A.href "http://wowprogramming.com/sputnik/login&next=docs%2Fapi" ! A.title "Login" $ H.text "Login"
                                , H.text "or"
                                , a ! A.href "http://wowprogramming.com/sputnik/register" ! A.title "register" $ H.text "register"]
                            , div ! A.id "navigation" $ ul </
                                [ li $ a ! A.href "http://wowprogramming.com/store" ! A.class_ "other" $ H.text "Store"
                                , li $ a ! A.href "http://wowprogramming.com/about" ! A.class_ "other" $ H.text "About"
                                , li $ a ! A.href "http://wowprogramming.com/utils" ! A.class_ "other" $ H.text "Utils"
                                , li $ a ! A.href "http://wowprogramming.com/snippets" ! A.class_ "other" $ H.text "Snippets"
                                , li $ a ! A.href "http://wowprogramming.com/docs" ! A.class_ "current" $ H.text "Reference"
                                , li $ a ! A.href "http://wowprogramming.com/forums" ! A.class_ "other" $ H.text "Forums"
                                , li $ a ! A.href "http://wowprogramming.com/" ! A.class_ "other" $ H.text "Home"]
                            ]
                        , div ! A.id "divider" $ div ! A.id "breadcrumb" $ ul </
                            [ li ! A.class_ "first" $ a ! A.href "http://wowprogramming.com/docs" $ H.text "Reference"
                            , li ! A.class_ "follow" $ a ! A.href "http://wowprogramming.com/docs/api" $ H.text "API Listing"]
                        , div ! A.id "toolbar" $ ul </
                            [ li $ a ! A.href "http://wowprogramming.com/docs/api.rss" ! A.title "RSS for edits to this node" $ 
                                img ! A.href "http://wowprogramming.com/images/rss_icon_small.png" ! A.alt "Button"]
                        , div ! A.id "content" </
                            [ table ! A.class_ "api-alpha" </
                                [ thead $ tr </
                                    [ th $ H.text "Name"
                                    , th $ H.text "Description"]
                                , genAPIListing]
                            ] -- content
                        , div ! A.id "footer" </
                            [ ul </ 
                                [ li ! A.class_ "first" $ a ! A.href "http://wowprogramming.com/about/privacy" $ H.text "Privacy Policy"
                                , li $ a ! A.href "http://wowprogramming.com/about/terms_of_service" $ H.text "Terms of Service"]
                            , p </
                                [ H.text "World of Warcraft"
                                , unescapedText "&trade;"
                                , H.text " and Blizzard Entertainment"
                                , unescapedText "&trade;"
                                , H.text "are trademarks or registered trademarks of Blizzard Entertainment, Inc. in the U.S. and/or other countries.  The original materials contained in the "
                                , em $ H.text "API Reference"
                                , H.text " section of this website are copyright John Wiley "
                                , unescapedText "&amp;"
                                , H.text " Sons.  Other copyrights on this page are owned by their respective owners.  All other content "
                                , unescapedText "&copy;"
                                , H.text " 2008-2009 "
                                , a ! A.href "/about" $ H.text "wowprogramming.com"
                                , H.text "."]
                            ] -- footer
                        ] -- wrapper
                    , script ! [A.type_ "text/javascript", ("src", "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js")] $ emptyText
                    , script ! [A.type_ "text/javascript", ("src", "http://wowprogramming.com/js/wowprogramming.js")] $ emptyText
                    , script ! [A.type_ "text/javascript", ("src", "http://wowprogramming.com/js/jquery.overlay-1.0.1.pack.js")] $ emptyText
                    , script ! A.type_ "text/javascript" $ 
                        unescapedText "/* ![CDATA[ */ $(function() { $(\"button[rel]\").overlay({ target: \"#debug_overlay\"});}) /* ]]> */"
                    , script ! A.type_ "text/javascript" $
                        unescapedText "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
                    , script ! A.type_ "text/javascript" $
                        unescapedText "try {var pageTracker = _gat._getTracker(\"UA-8605143-1\"); pageTracker._trackPageview();} catch(err) {}"
                    ] -- body
            ] -- html

genAPIRow :: (Html h) => (Text, Text, [Text]) -> h
genAPIRow (name, desc, badges) = tr </ [lcol, rcol]
  where
    lcol = td </ [H.a ! href $ H.text name, genAPIBadges badges]
    rcol = td $ unescapedText desc
    href = A.href $ "http://wowprogramming.com/docs/api/" `mappend` name

genAPIBadges :: (Html h) => [Text] -> h
genAPIBadges [] = emptyText
genAPIBadges x = ul ! ("class"::Text, "api_badge"::Text) </ (genAPIBadgeInner x)

genAPIBadgeInner :: (Html h) => [Text] -> [h]
genAPIBadgeInner [] = []
genAPIBadgeInner (x:xs) = [li ! A.class_ "api_badge" $ a ! href $ H.text x] ++ (genAPIBadgeInner xs)
  where
    href = A.href $ "http://wowprogramming.com/docs/api_flags#" `mappend` x

genAPIListing :: (Html h) => h
genAPIListing = mconcat . P.map genAPIRow $ dataAPI

runBenchmarks :: IO ()
runBenchmarks = defaultMain [
    bgroup "simple"
        [ bench "render_byteString" $ render_byteString 1000
        , bench "render_text" $ render_text 1000
        , bench "render_prettyText" $ render_prettyText 1000
        ]
    ]

render_byteString :: Int -> IO ()
render_byteString 0 = return ()
render_byteString n = (htmlByteString exampleContent) `seq` render_byteString (n - 1)

render_prettyText :: Int -> IO ()
render_prettyText 0 = return () 
render_prettyText n = (htmlPrettyText exampleContent) `seq` render_prettyText (n - 1)

render_text :: Int -> IO ()
render_text 0 = return () 
render_text n = (htmlText exampleContent) `seq` render_text (n - 1)

-- | Use the benchmark definition to run benchmarks
main :: IO ()
--main = runBenchmarks
main = putStr $ htmlByteString $ indented $ exampleContent
