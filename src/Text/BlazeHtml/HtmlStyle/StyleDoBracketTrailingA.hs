{-# LANGUAGE OverloadedStrings #-}
module StyleDoBracketTrailingA where

import Prelude hiding (div,head,span,putStr)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlPrettyText
import Data.Monoid
import Data.Text.IO (putStr)
--(html,head,title,link,body,div,form,input,a,ul,li,text,unescapedText,img,p,script,(<!),(!),(<!:))

-- Renamed combinators for trailing A's
abbrA = atAbbr
accept_charsetA = atAccept_charset
acceptA = atAccept
accesskeyA = atAccesskey
actionA = atAction
alignA = atAlign
alinkA = atAlink
altA = atAlt
archiveA = atArchive
axisA = atAxis
backgroundA = atBackground
bgcolorA = atBgcolor
borderA = atBorder
cellpaddingA = atCellpadding
cellspacingA = atCellspacing
charA = atChar
charoffA = atCharoff
charsetA = atCharset
checkedA = atChecked
citeA = atCite
classA = atClass
classidA = atClassid
clearA = atClear
codeA = atCode
codebaseA = atCodebase
codetypeA = atCodetype
colorA = atColor
colsA = atCols
colspanA = atColspan
compactA = atCompact
contentA = atContent
coordsA = atCoords
dataA = atData
datetimeA = atDatetime
declareA = atDeclare
deferA = atDefer
dirA = atDir
disabledA = atDisabled
enctypeA = atEnctype
faceA = atFace
forA = atFor
frameA = atFrame
frameborderA = atFrameborder
headersA = atHeaders
heightA = atHeight
hrefA = atHref
hreflangA = atHreflang
hspaceA = atHspace
http_equivA = atHttp_equiv
idA = atId
ismapA = atIsmap
labelA = atLabel
langA = atLang
languageA = atLanguage
linkA = atLink
longdescA = atLongdesc
marginheightA = atMarginheight
marginwidthA = atMarginwidth
maxlengthA = atMaxlength
mediaA = atMedia
methodA = atMethod
multipleA = atMultiple
nameA = atName
nohrefA = atNohref
noresizeA = atNoresize
noshadeA = atNoshade
nowrapA = atNowrap
objectA = atObject
onblurA = atOnblur
onchangeA = atOnchange
onclickA = atOnclick
ondblclickA = atOndblclick
onfocusA = atOnfocus
onkeydownA = atOnkeydown
onkeypressA = atOnkeypress
onkeyupA = atOnkeyup
onloadA = atOnload
onmousedownA = atOnmousedown
onmousemoveA = atOnmousemove
onmouseoutA = atOnmouseout
onmouseoverA = atOnmouseover
onmouseupA = atOnmouseup
onresetA = atOnreset
onselectA = atOnselect
onsubmitA = atOnsubmit
onunloadA = atOnunload
profileA = atProfile
promptA = atPrompt
readonlyA = atReadonly
relA = atRel
revA = atRev
rowsA = atRows
rowspanA = atRowspan
rulesA = atRules
schemeA = atScheme
scopeA = atScope
scrollingA = atScrolling
selectedA = atSelected
shapeA = atShape
sizeA = atSize
spanA = atSpan
srcA = atSrc
standbyA = atStandby
startA = atStart
styleA = atStyle
summaryA = atSummary
tabindexA = atTabindex
targetA = atTarget
textA = atText
titleA = atTitle
typeA = atType
usemapA = atUsemap
valignA = atValign
valueA = atValue
valuetypeA = atValuetype
versionA = atVersion
vlinkA = atVlink
vspaceA = atVspace
widthA = atWidth

--content :: (Html h) => h
content = html <! ("xmlns", "http://www.w3.org/1999/xhtml") $ do 
            head $ do 
              title $ text "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
              link !: [typeA "text/css", relA "stylesheet", hrefA "http://wowprogramming.com/css/blueprint/reset.css", mediaA "screen, projection"]
              link !: [typeA "text/css", relA "stylesheet", hrefA "http://wowprogramming.com/css/blueprint/typography.css", mediaA "screen, projection"]
              link !: [typeA "text/css", relA "stylesheet", hrefA "http://wowprogramming.com/css/blueprint/print.css", mediaA "print"]
              link !: [typeA "text/css", relA "stylesheet", hrefA "http://wowprogramming.com/css/wowprogramming.css", mediaA "screen, projection"]
              link !: [relA "shortcut icon", hrefA "http://wowprogramming.com/images/favicon.ico"]
              link !: [relA "alternate", typeA "application/rss+xml", titleA "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons",
                       hrefA "http://wowprogramming.com/history.rss"]
              link !: [relA "alternate", typeA "application/rss+xml",
                       titleA "Recent edits to World of Warcraft Programming: A Guide and Reference for Creating WoW Addons: docs/api",
                       hrefA "http://wowprogramming.com/docs/api.rss"]
            body $ do 
              div <! idA "wrapper" $ do 
                     div <! idA "header" $ do 
                                        div <! idA "search" $ do 
                                            form <!: [idA "search_form", actionA "http://google.com/search"] $ do 
                                                                input !: [idA "search_box", typeA "text", nameA "q", valueA "Search using Google"]
                                                                input !: [typeA "hidden", nameA "as_sitesearch", valueA "wowprogramming.com"]
                                                                input !: [idA "search_button", typeA "submit", valueA ""]
                                        div <! idA "login_status" $ do 
                                                                       a "http://wowprogramming.com/sputnik/login&next=docs%2Fapi" <! titleA "Login" $ text "Login"
                                                                       text "or"
                                                                       a "http://wowprogramming.com/sputnik/register" <! titleA "register" $ text "register"
                                        div <! idA "navigation" $ ul $ do 
                                                                          li $ a "http://wowprogramming.com/store" <! classA "other" $ text "Store"
                                                                          li $ a "http://wowprogramming.com/about" <! classA "other" $ text "About"
                                                                          li $ a "http://wowprogramming.com/utils" <! classA "other" $ text "Utils"
                                                                          li $ a "http://wowprogramming.com/snippets" <! classA "other" $ text "Snippets"
                                                                          li $ a "http://wowprogramming.com/docs" <! classA "current" $ text "Reference"
                                                                          li $ a "http://wowprogramming.com/forums" <! classA "other" $ text "Forums"
                                                                          li $ a "http://wowprogramming.com/" <! classA "other" $ text "Home"
                     div <! idA "divider" $ div <! idA "breadcrumb" $ ul $ do 
                                                                               li <! classA "first" $ a "http://wowprogramming.com/docs" $ text "Reference"
                                                                               li <! classA "follow" $ a "http://wowprogramming.com/docs/api" $ text "API Listing" 
                     div <! idA "toolbar" $ ul $ do
                                        li $ a "http://wowprogramming.com/docs/api.rss" <! titleA "RSS for edits to this node" $ img "http://wowprogramming.com/images/rss_icon_small.png" "Button"
                     div <! idA "content" $ emptyText -- content
                     div <! idA "footer" $ do 
                                              ul $ do 
                                                li <! classA "first" $ a "http://wowprogramming.com/about/privacy" $ text "Privacy Policy"
                                                li $ a "http://wowprogramming.com/about/terms_of_service" $ text "Terms of Service" 
                                              p $ text "World of Warcraft™ and Blizzard Entertainment™ are trademarks or registered trademarks \
                                                        \of Blizzard Entertainment, Inc. in the U.S. and/or other countries.  The original materials \
                                                        \contained in the" |-| (em $ text "API Reference") |-| text "section of this website are \
                                                        \copyright John Wiley & Sons.  Other copyrights on this page are owned by their respective \
                                                        \owners.  All other content © 2008-2009" |-| (a "/about" $ text "wowprogramming.com") |-| text "."
              script <!: [typeA "text/javascript", srcA "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"] $ emptyText
              script <!: [typeA "text/javascript", srcA "http://wowprogramming.com/js/wowprogramming.js"] $ emptyText
              script <!: [typeA "text/javascript", srcA "http://wowprogramming.com/js/jquery.overlay-1.0.1.pack.js"] $ emptyText
              script <! typeA "text/javascript" $ unescapedText "/* <![CDATA[ */ $(function() { $(\"button[rel]\").overlay({ target: \"#debug_overlay\"});}) /* ]]> */"
              script <! typeA "text/javascript" $ unescapedText "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\"); document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
              script <! typeA "text/javascript" $ unescapedText "try {var pageTracker = _gat._getTracker(\"UA-8605143-1\"); pageTracker._trackPageview();} catch(err) {}"

testPrint = putStr $ htmlPrettyText $ runHtmlMonad content


