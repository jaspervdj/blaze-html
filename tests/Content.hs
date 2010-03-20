{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, head, span, putStr)
import Text.BlazeHtml.Html
import Text.BlazeHtml.Render.HtmlPrettyText
import Text.BlazeHtml.Text (putStr)

foo = "hello"

bar :: (Html h) => h
bar = text "monkey"

content :: (Html h) => h
content = html <! ("xmlns", "http://www.w3.org/1999/xhtml") </ 
            [ head </ 
                [ title $ text "docs/api - World of Warcraft Programming: A Guide and Reference for Creating WoW Addons"
                , link !: [ ("type", "text/css"), ("rel", "stylesheet"), ("href", "/css/blueprint/reset.css")
                          , ("media", "screen, projection")]]
                , body </ [ h1 $ text "Hello!"
                          , p $ text "Welcome to BlazeHtml." ]]

testPrint = putStr $ renderHtmlPrettyText content
