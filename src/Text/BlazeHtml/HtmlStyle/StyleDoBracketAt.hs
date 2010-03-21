{-# LANGUAGE OverloadedStrings #-}
module StyleDoBracketAt where

import Prelude hiding (div,head,span,putStr)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlPrettyText
import Data.Monoid
import Data.Text.IO (putStr)
--(html,head,title,link,body,div,form,input,a,ul,li,text,unescapedText,img,p,script,(<!),(!),(<!:))

(<>) :: Monoid h => h -> h -> h
(<>) = mappend

--(|-|) :: Html h => h -> h -> h
a |-| b = a <> space <> b

space = text " "

--content :: (Html h) => h
content = html <! ("xmlns", "http://www.w3.org/1999/xhtml") $ do 
            head $ do 
              title $ text "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
              link !: [atType "text/css", atRel "stylesheet", atHref "http://wowprogramming.com/css/blueprint/reset.css", atMedia "screen, projection"]
              link !: [atType "text/css", atRel "stylesheet", atHref "http://wowprogramming.com/css/blueprint/typography.css", atMedia "screen, projection"]
              link !: [atType "text/css", atRel "stylesheet", atHref "http://wowprogramming.com/css/blueprint/print.css", atMedia "print"]
              link !: [atType "text/css", atRel "stylesheet", atHref "http://wowprogramming.com/css/wowprogramming.css", atMedia "screen, projection"]
              link !: [atRel "shortcut icon", atHref "http://wowprogramming.com/images/favicon.ico"]
              link !: [atRel "alternate", atType "application/rss+xml", atTitle "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons",
                       atHref "http://wowprogramming.com/history.rss"]
              link !: [atRel "alternate", atType "application/rss+xml",
                       atTitle "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons: docs/api",
                       atHref "http://wowprogramming.com/docs/api.rss"]
            body $ do 
              div <! atId "wrapper" $ do 
                     div <! atId "header" $ do 
                                        div <! atId "search" $ do 
                                            form <!: [atId "search_form", atAction "http://google.com/search"] $ do 
                                                                input !: [atId "search_box", atType "text", atName "q", atValue "Search using Google"]
                                                                input !: [atType "hidden", atName "as_sitesearch", atValue "wowprogramming.com"]
                                                                input !: [atId "search_button", atType "submit", atValue ""]
                                        div <! atId "login_status" $ do 
                                                                       a "http://wowprogramming.com/sputnik/login&next=docs%2Fapi" <! atTitle "Login" $ text "Login"
                                                                       text "or"
                                                                       a "http://wowprogramming.com/sputnik/register" <! atTitle "register" $ text "register"
                                        div <! atId "navigation" $ ul $ do 
                                                                          li $ a "http://wowprogramming.com/store" <! atClass "other" $ text "Store"
                                                                          li $ a "http://wowprogramming.com/about" <! atClass "other" $ text "About"
                                                                          li $ a "http://wowprogramming.com/utils" <! atClass "other" $ text "Utils"
                                                                          li $ a "http://wowprogramming.com/snippets" <! atClass "other" $ text "Snippets"
                                                                          li $ a "http://wowprogramming.com/docs" <! atClass "current" $ text "Reference"
                                                                          li $ a "http://wowprogramming.com/forums" <! atClass "other" $ text "Forums"
                                                                          li $ a "http://wowprogramming.com/" <! atClass "other" $ text "Home"
                     div <! atId "divider" $ div <! atId "breadcrumb" $ ul $ do 
                                                                               li <! atClass "first" $ a "http://wowprogramming.com/docs" $ text "Reference"
                                                                               li <! atClass "follow" $ a "http://wowprogramming.com/docs/api" $ text "API Listing" 
                     div <! atId "toolbar" $ ul $ do
                                        li $ a "http://wowprogramming.com/docs/api.rss" <! atTitle "RSS for edits to this node" $ img "http://wowprogramming.com/images/rss_icon_small.png" "Button"
                     div <! atId "content" $ emptyText -- content
                     div <! atId "footer" $ do 
                                              ul $ do 
                                                li <! atClass "first" $ a "http://wowprogramming.com/about/privacy" $ text "Privacy Policy"
                                                li $ a "http://wowprogramming.com/about/terms_of_service" $ text "Terms of Service" 
                                              p $ text "World of Warcraft™ and Blizzard Entertainment™ are trademarks or registered trademarks \
                                                        \of Blizzard Entertainment, Inc. in the U.S. and/or other countries.  The original materials \
                                                        \contained in the" |-| (em $ text "API Reference") |-| text "section of this website are \
                                                        \copyright John Wiley & Sons.  Other copyrights on this page are owned by their respective \
                                                        \owners.  All other content © 2008-2009" |-| (a "/about" $ text "wowprogramming.com") |-| text "."
              script <!: [atType "text/javascript", atSrc "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"] $ emptyText
              script <!: [atType "text/javascript", atSrc "http://wowprogramming.com/js/wowprogramming.js"] $ emptyText
              script <!: [atType "text/javascript", atSrc "http://wowprogramming.com/js/jquery.overlay-1.0.1.pack.js"] $ emptyText
              script <! atType "text/javascript" $ unescapedText "/* <![CDATA[ */ $(function() { $(\"button[rel]\").overlay({ target: \"#debug_overlay\"});}) /* ]]> */"
              script <! atType "text/javascript" $ unescapedText "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
              script <! atType "text/javascript" $ unescapedText "try {var pageTracker = _gat._getTracker(\"UA-8605143-1\"); pageTracker._trackPageview();} catch(err) {}"

testPrint = putStr $ htmlPrettyText $ runHtmlMonad content