{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, head, span, putStr)
import qualified Prelude as P
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlByteString
import Text.BlazeHtml.Text (Text)
import Data.ByteString.Lazy (putStr)
import Data.Monoid (mappend, mconcat)
import BlazeHtmlContentData (dataAPI)

content :: (Html h) => h
content = html <! ("xmlns", "http://www.w3.org/1999/xhtml") </ 
            [ head </ 
                [ title $ text "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
                , link !: [ ("type", "text/css"), ("rel", "stylesheet"), ("href", "http://wowprogramming.com/css/blueprint/reset.css")
                          , ("media", "screen, projection")]
                , link !: [ ("type", "text/css"), ("rel", "stylesheet"), ("href", "http://wowprogramming.com/css/blueprint/typography.css")
                          , ("media", "screen, projection")]
                , link !: [ ("type", "text/css"), ("rel", "stylesheet"), ("href", "http://wowprogramming.com/css/blueprint/print.css")
                          , ("media", "print")]
                , link !: [ ("type", "text/css"), ("rel", "stylesheet"), ("href", "http://wowprogramming.com/css/wowprogramming.css")
                          , ("media", "screen, projection")]
                , link !: [ ("rel", "shortcut icon"), ("href", "http://wowprogramming.com/images/favicon.ico")]
                , link !: [ ("rel", "alternate"), ("type", "application/rss+xml")
                          , ("title", "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons")
                          , ("href", "http://wowprogramming.com/history.rss")]
                , link !: [ ("rel", "alternate"), ("type", "application/rss+xml")
                          , ("title", "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons: docs/api")
                          , ("href", "http://wowprogramming.com/docs/api.rss")]
                ]

                , body </
                    [ div <! ("id", "wrapper") </
                        [ div <! ("id", "header") </
                            [ div <! ("id", "search") $ 
                                form <!: [("id", "search_form"), ("action", "http://google.com/search")] </
                                    [ input !: [("id", "search_box"), ("type", "text"), ("name", "q"), ("value", "Search using Google")]
                                    , input !: [("type", "hidden"), ("name", "as_sitesearch"), ("value", "wowprogramming.com")]
                                    , input !: [("id", "search_button"), ("type", "submit"), ("value", "")]]
                            , div <! ("id", "login_status") </
                                [ a "http://wowprogramming.com/sputnik/login&next=docs%2Fapi" <! ("title", "Login") $ text "Login"
                                , text "or"
                                , a "http://wowprogramming.com/sputnik/register" <! ("title", "register") $ text "register"]
                            , div <! ("id", "navigation") $ ul </
                                [ li $ a "http://wowprogramming.com/store" <! ("class", "other") $ text "Store"
                                , li $ a "http://wowprogramming.com/about" <! ("class", "other") $ text "About"
                                , li $ a "http://wowprogramming.com/utils" <! ("class", "other") $ text "Utils"
                                , li $ a "http://wowprogramming.com/snippets" <! ("class", "other") $ text "Snippets"
                                , li $ a "http://wowprogramming.com/docs" <! ("class", "current") $ text "Reference"
                                , li $ a "http://wowprogramming.com/forums" <! ("class", "other") $ text "Forums"
                                , li $ a "http://wowprogramming.com/" <! ("class", "other") $ text "Home"]
                            ]
                        , div <! ("id", "divider") $ div <! ("id", "breadcrumb") $ ul </
                            [ li <! ("class", "first") $ a "http://wowprogramming.com/docs" $ text "Reference"
                            , li <! ("class", "follow") $ a "http://wowprogramming.com/docs/api" $ text "API Listing"]
                        , div <! ("id", "toolbar") $ ul </
                            [ li $ a "http://wowprogramming.com/docs/api.rss" <! ("title", "RSS for edits to this node") $ 
                                img "http://wowprogramming.com/images/rss_icon_small.png" "Button"]
                        , div <! ("id", "content") </
                            [ table <! ("class", "api-alpha") </
                                [ thead $ tr </
                                    [ th $ text "Name"
                                    , th $ text "Description"]
                                , genAPIListing]
                            ] -- content
                        , div <! ("id", "footer") </
                            [ ul </ 
                                [ li <! ("class", "first") $ a "http://wowprogramming.com/about/privacy" $ text "Privacy Policy"
                                , li $ a "http://wowprogramming.com/about/terms_of_service" $ text "Terms of Service"]
                            , p </
                                [ text "World of Warcraft"
                                , unescapedText "&trade;"
                                , text " and Blizzard Entertainment"
                                , unescapedText "&trade;"
                                , text "are trademarks or registered trademarks of Blizzard Entertainment, Inc. in the U.S. and/or other countries.  The original materials contained in the "
                                , em $ text "API Reference"
                                , text " section of this website are copyright John Wiley "
                                , unescapedText "&amp;"
                                , text " Sons.  Other copyrights on this page are owned by their respective owners.  All other content "
                                , unescapedText "&copy;"
                                , text " 2008-2009 "
                                , a "/about" $ text "wowprogramming.com"
                                , text "."]
                            ] -- footer
                        ] -- wrapper
                    , script <!: [("type", "text/javascript"), ("src", "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js")] $ emptyText
                    , script <!: [("type", "text/javascript"), ("src", "http://wowprogramming.com/js/wowprogramming.js")] $ emptyText
                    , script <!: [("type", "text/javascript"), ("src", "http://wowprogramming.com/js/jquery.overlay-1.0.1.pack.js")] $ emptyText
                    , script <! ("type", "text/javascript") $ 
                        unescapedText "/* <![CDATA[ */ $(function() { $(\"button[rel]\").overlay({ target: \"#debug_overlay\"});}) /* ]]> */"
                    , script <! ("type", "text/javascript") $
                        unescapedText "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
                    , script <! ("type", "text/javascript") $
                        unescapedText "try {var pageTracker = _gat._getTracker(\"UA-8605143-1\"); pageTracker._trackPageview();} catch(err) {}"
                    ] -- body
            ] -- html

genAPIRow :: (Html h) => (Text, Text, [Text]) -> h
genAPIRow (name, desc, badges) = tr </ [lcol, rcol]
td $ a href $ text name, td text desc, (genAPIBadges badges)]
  where
    href = "http://wowprogramming.com/docs/api/" `mappend` name

genAPIBadges :: (Html h) => [Text] -> h
genAPIBadges [] = emptyText
genAPIBadges x = ul <! ("class", "api_badge") </ (genAPIBadgeInner x)

genAPIBadgeInner :: (Html h) => [Text] -> [h]
genAPIBadgeInner [] = []
genAPIBadgeInner (x:xs) = [li <! ("class", "api_badge") $ a href $ text x] ++ (genAPIBadgeInner xs)
  where
    href = "http://wowprogramming.com/docs/api_flags#" `mappend` x

genAPIListing :: (Html h) => h
genAPIListing = mconcat . P.map genAPIRow $ dataAPI

main :: IO ()
main = do
    putStr $ htmlByteString content
