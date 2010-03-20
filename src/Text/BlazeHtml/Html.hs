{-# LANGUAGE OverloadedStrings #-}
module Text.BlazeHtml.Html
    ( module Text.BlazeHtml.Internal.Html
    , img
    , a
    , big
    , blockquote
    , body
    , b
    , caption
    , center
    , cite
    , em
    , form
    , frame
    , frameset
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , head
    , i
    , li
    , noframes
    , ol
    , p
    , pre
    , small
    , strong
    , style
    , span
    , sub
    , sup
    , table
    , td
    , textarea
    , th
    , code
    , div
    , html
    , link
    , title
    , tr
    , ul
    , u
    ) where

import Prelude hiding (div, head, span)

import Text.BlazeHtml.Text (Text)
import Text.BlazeHtml.Internal.Html 
    hiding (modifyUnescapedAttributes, clearAttributes)

-- | This is an auxiliary function to fix the type of attributes, because type
--   inference comes up with too general types.
attrs :: [(Text, Text)] -> [(Text, Text)]
attrs = id

-- | This is a function comparable to @attrs@, but supporting one argument.
attr :: (Text, Text) -> (Text, Text)
attr = id

-- | Render an @img@ element.
--
-- > img "foo.png" "Foo Illustration"
img :: (Html h) => Text -> Text -> h
img src alt = renderLeafElement "img" ! attrs [("src", src), ("alt", alt)]

-- | Render an @a@ element.
--
-- > a "index.html"
a :: (Html h) => Text -> h -> h
a href = renderElement "a" <! attr ("href", href)

-- | Render a @big@ element.
big :: (Html h) => h -> h
big = renderElement "big"

-- | Render a @blockquote@ element.
blockquote :: (Html h) => h -> h
blockquote = renderElement "blockquote"

-- | Render a @body@ element.
body :: (Html h) => h -> h
body = renderElement "body"

-- | Render a @b@ element.
b :: (Html h) => h -> h
b = renderElement "b"

-- | Render a @caption@ element.
caption :: (Html h) => h -> h
caption = renderElement "caption"

-- | Render a @center@ element.
center :: (Html h) => h -> h
center = renderElement "center"

-- | Render a @cite@ element.
cite :: (Html h) => h -> h
cite = renderElement "cite"

-- | Render a @em@ element.
em :: (Html h) => h -> h
em = renderElement "em"

-- | Render a @form@ element.
form :: (Html h) => h -> h
form = renderElement "form"

-- | Render a @frame@ element.
frame :: (Html h) => h -> h
frame = renderElement "frame"

-- | Render a @frameset@ element.
frameset :: (Html h) => h -> h
frameset = renderElement "frameset"

-- | Render a @h1@ element.
h1 :: (Html h) => h -> h
h1 = renderElement "h1"

-- | Render a @h2@ element.
h2 :: (Html h) => h -> h
h2 = renderElement "h2"

-- | Render a @h3@ element.
h3 :: (Html h) => h -> h
h3 = renderElement "h3"

-- | Render a @h4@ element.
h4 :: (Html h) => h -> h
h4 = renderElement "h4"

-- | Render a @h5@ element.
h5 :: (Html h) => h -> h
h5 = renderElement "h5"

-- | Render a @h6@ element.
h6 :: (Html h) => h -> h
h6 = renderElement "h6"

-- | Render a @head@ element.
head :: (Html h) => h -> h
head = renderElement "head"

-- | Render a @i@ element.
i :: (Html h) => h -> h
i = renderElement "i"

-- | Render a @li@ element.
li :: (Html h) => h -> h
li = renderElement "li"

-- | Render a @noframes@ element.
noframes :: (Html h) => h -> h
noframes = renderElement "noframes"

-- | Render a @ol@ element.
ol :: (Html h) => h -> h
ol = renderElement "ol"

-- | Render a @p@ element.
p :: (Html h) => h -> h
p = renderElement "p"

-- | Render a @pre@ element.
pre :: (Html h) => h -> h
pre = renderElement "pre"

-- | Render a @small@ element.
small :: (Html h) => h -> h
small = renderElement "small"

-- | Render a @strong@ element.
strong :: (Html h) => h -> h
strong = renderElement "strong"

-- | Render a @style@ element.
style :: (Html h) => h -> h
style = renderElement "style"

-- | Render a @span@ element.
span :: (Html h) => h -> h
span = renderElement "span"

-- | Render a @sub@ element.
sub :: (Html h) => h -> h
sub = renderElement "sub"

-- | Render a @sup@ element.
sup :: (Html h) => h -> h
sup = renderElement "sup"

-- | Render a @table@ element.
table :: (Html h) => h -> h
table = renderElement "table"

-- | Render a @td@ element.
td :: (Html h) => h -> h
td = renderElement "td"

-- | Render a @textarea@ element.
textarea :: (Html h) => h -> h
textarea = renderElement "textarea"

-- | Render a @th@ element.
th :: (Html h) => h -> h
th = renderElement "th"

-- | Render a @code@ element.
code :: (Html h) => h -> h
code = renderElement "code"

-- | Render a @div@ element.
div :: (Html h) => h -> h
div = renderElement "div"

-- | Render a @html@ element.
html :: (Html h) => h -> h
html = renderElement "html"

-- | Render a @link@ element.
link :: (Html h) => h -> h
link = renderElement "link"

-- | Render a @title@ element.
title :: (Html h) => h -> h
title = renderElement "title"

-- | Render a @tr@ element.
tr :: (Html h) => h -> h
tr = renderElement "tr"

-- | Render a @ul@ element.
ul :: (Html h) => h -> h
ul = renderElement "ul"

-- | Render a @u@ element.
u :: (Html h) => h -> h
u = renderElement "u"