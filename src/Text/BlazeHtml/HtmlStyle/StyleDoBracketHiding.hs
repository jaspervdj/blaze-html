{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.HtmlStyle.StyleDoBracketHiding where

import Prelude hiding (div,head,span,putStr)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlPrettyText
import Data.Monoid
import Data.Text.IO (putStr)
import qualified Text.BlazeHtml.HtmlStyle.StyleDoBracketHidingAttributes as A

-- Renamed combinators for trailing A's

-- typeA relA hrefA mediaA titleA idA actionA valueA nameA classA srcA


--content :: (Html h) => h
content = html <! ("xmlns", "http://www.w3.org/1999/xhtml") $ do 
            head $ do 
              title $ text "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
              link !: [A.type' "text/css", A.rel "stylesheet", A.href "http://wowprogramming.com/css/blueprint/reset.css", A.media "screen, projection"]
              link !: [A.type' "text/css", A.rel "stylesheet", A.href "http://wowprogramming.com/css/blueprint/typography.css", A.media "screen, projection"]
              link !: [A.type' "text/css", A.rel "stylesheet", A.href "http://wowprogramming.com/css/blueprint/print.css", A.media "print"]
              link !: [A.type' "text/css", A.rel "stylesheet", A.href "http://wowprogramming.com/css/wowprogramming.css", A.media "screen, projection"]
              link !: [A.rel "shortcut icon", A.href "http://wowprogramming.com/images/favicon.ico"]
              link !: [A.rel "alternate", A.type' "application/rss+xml", A.title "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons",
                       A.href "http://wowprogramming.com/history.rss"]
              link !: [A.rel "alternate", A.type' "application/rss+xml",
                       A.title "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons: docs/api",
                       A.href "http://wowprogramming.com/docs/api.rss"]
            body $ do 
              div <! A.id "wrapper" $ do 
                     div <! A.id "header" $ do 
                                        div <! A.id "search" $ do 
                                            form <!: [A.id "search_form", A.action "http://google.com/search"] $ do 
                                                                input !: [A.id "search_box", A.type' "text", A.name "q", A.value "Search using Google"]
                                                                input !: [A.type' "hidden", A.name "as_sitesearch", A.value "wowprogramming.com"]
                                                                input !: [A.id "search_button", A.type' "submit", A.value ""]
                                        div <! A.id "login_status" $ do 
                                                                       a "http://wowprogramming.com/sputnik/login&next=docs%2Fapi" <! A.title "Login" $ text "Login"
                                                                       text "or"
                                                                       a "http://wowprogramming.com/sputnik/register" <! A.title "register" $ text "register"
                                        div <! A.id "navigation" $ ul $ do 
                                                                          li $ a "http://wowprogramming.com/store" <! A.class' "other" $ text "Store"
                                                                          li $ a "http://wowprogramming.com/about" <! A.class' "other" $ text "About"
                                                                          li $ a "http://wowprogramming.com/utils" <! A.class' "other" $ text "Utils"
                                                                          li $ a "http://wowprogramming.com/snippets" <! A.class' "other" $ text "Snippets"
                                                                          li $ a "http://wowprogramming.com/docs" <! A.class' "current" $ text "Reference"
                                                                          li $ a "http://wowprogramming.com/forums" <! A.class' "other" $ text "Forums"
                                                                          li $ a "http://wowprogramming.com/" <! A.class' "other" $ text "Home"
                     div <! A.id "divider" $ div <! A.id "breadcrumb" $ ul $ do 
                                                                               li <! A.class' "first" $ a "http://wowprogramming.com/docs" $ text "Reference"
                                                                               li <! A.class' "follow" $ a "http://wowprogramming.com/docs/api" $ text "API Listing" 
                     div <! A.id "toolbar" $ ul $ do
                                        li $ a "http://wowprogramming.com/docs/api.rss" <! A.title "RSS for edits to this node" $ img "http://wowprogramming.com/images/rss_icon_small.png" "Button"
                     div <! A.id "content" $ emptyText -- content
                     div <! A.id "footer" $ do 
                                              ul $ do 
                                                li <! A.class' "first" $ a "http://wowprogramming.com/about/privacy" $ text "Privacy Policy"
                                                li $ a "http://wowprogramming.com/about/terms_of_service" $ text "Terms of Service" 
                                              p $ text "World of Warcraft™ and Blizzard Entertainment™ are trademarks or registered trademarks \
                                                        \of Blizzard Entertainment, Inc. in the U.S. and/or other countries.  The original materials \
                                                        \contained in the" |-| (em $ text "API Reference") |-| text "section of this website are \
                                                        \copyright John Wiley & Sons.  Other copyrights on this page are owned by their respective \
                                                        \owners.  All other content © 2008-2009" |-| (a "/about" $ text "wowprogramming.com") |-| text "."
              script <!: [A.type' "text/javascript", A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"] $ emptyText
              script <!: [A.type' "text/javascript", A.src "http://wowprogramming.com/js/wowprogramming.js"] $ emptyText
              script <!: [A.type' "text/javascript", A.src "http://wowprogramming.com/js/jquery.overlay-1.0.1.pack.js"] $ emptyText
              script <! A.type' "text/javascript" $ unescapedText "/* <![CDATA[ */ $(function() { $(\"button[rel]\").overlay({ target: \"#debug_overlay\"});}) /* ]]> */"
              script <! A.type' "text/javascript" $ unescapedText "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
              script <! A.type' "text/javascript" $ unescapedText "try {var pageTracker = _gat._getTracker(\"UA-8605143-1\"); pageTracker._trackPageview();} catch(err) {}"

testPrint = putStr $ htmlPrettyText $ runHtmlMonad content


