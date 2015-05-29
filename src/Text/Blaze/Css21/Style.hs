{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Css21.Style
    ( backgroundAttachment
    , backgroundColor
    , backgroundImage
    , backgroundPosition
    , backgroundRepeat
    , background
    , borderCollapse
    , borderColor
    , borderSpacing
    , borderStyle
    , borderTop
    , borderRight
    , borderBottom
    , borderLeft
    , borderTopColor
    , borderRightColor
    , borderBottomColor
    , borderLeftColor
    , borderTopStyle
    , borderRightStyle
    , borderBottomStyle
    , borderLeftStyle
    , borderTopWidth
    , borderRightWidth
    , borderBottomWidth
    , borderLeftWidth
    , borderWidth
    , border
    , captionSide
    , clear
    , clip
    , color
    , cursor
    , direction
    , display
    , emptyCells
    , float
    , fontFamily
    , fontSize
    , fontStyle
    , fontVariant
    , fontWeight
    , height
    , left
    , letterSpacing
    , lineHeight
    , listStyleImage
    , listStylePosition
    , listStyleType
    , marginLeft
    , marginRight
    , marginTop
    , marginBottom
    , margin
    , maxHeight
    , maxWidth
    , minHeight
    , minWidth
    , orphans
    , outlineColor
    , outlineStyle
    , outlineWidth
    , outline
    , overflow
    , paddingTop
    , paddingRight
    , paddingBottom
    , paddingLeft
    , padding
    , pageBreakAfter
    , pageBreakBefore
    , pageBreakInside
    , position
    , right
    , tableLayout
    , textAlign
    , textDecoration
    , textIndent
    , textTransform
    , top
    , unicodeBiDi
    , verticalAlign
    , visibility
    , whitespace
    , width
    , widows
    , wordSpacing
    , zIndex
    )
where

import qualified    Text.Blaze.Internal as B
import              Text.Blaze.Css21.Value


backgroundAttachment :: Value -> B.Attribute
backgroundAttachment = B.cssStyle "background-attachment" "background-attachment: " . B.stringValue . toString

backgroundColor :: Value -> B.Attribute
backgroundColor = B.cssStyle "background-color" "background-color: " . B.stringValue . toString

backgroundImage :: Value -> B.Attribute
backgroundImage = B.cssStyle "background-image" "background-image: " . B.stringValue . toString

backgroundPosition :: Value -> B.Attribute
backgroundPosition = B.cssStyle "background-position" "background-position: " . B.stringValue . toString

backgroundRepeat :: Value -> B.Attribute
backgroundRepeat = B.cssStyle "background-repeat" "background-repeat: " . B.stringValue . toString

background :: Value -> B.Attribute
background = B.cssStyle "background" "background: " . B.stringValue . toString

borderCollapse :: Value -> B.Attribute
borderCollapse = B.cssStyle "border-collapse" "border-collapse: " . B.stringValue . toString

borderColor :: Value -> B.Attribute
borderColor = B.cssStyle "border-color" "border-color: " . B.stringValue . toString

borderSpacing :: Value -> B.Attribute
borderSpacing = B.cssStyle "border-spacing" "border-spacing: " . B.stringValue . toString

borderStyle :: Value -> B.Attribute
borderStyle = B.cssStyle "border-style" "border-style: " . B.stringValue . toString

borderTop :: Value -> B.Attribute
borderTop = B.cssStyle "border-top" "border-top: " . B.stringValue . toString

borderRight :: Value -> B.Attribute
borderRight = B.cssStyle "border-right" "border-right: " . B.stringValue . toString

borderBottom :: Value -> B.Attribute
borderBottom = B.cssStyle "border-bottom" "border-bottom: " . B.stringValue . toString

borderLeft :: Value -> B.Attribute
borderLeft = B.cssStyle "border-left" "border-left: " . B.stringValue . toString

borderTopColor :: Value -> B.Attribute
borderTopColor = B.cssStyle "border-top-color" "border-top-color: " . B.stringValue . toString

borderRightColor :: Value -> B.Attribute
borderRightColor = B.cssStyle "border-right-color" "border-right-color: " . B.stringValue . toString

borderBottomColor :: Value -> B.Attribute
borderBottomColor = B.cssStyle "border-bottom-color" "border-bottom-color: " . B.stringValue . toString

borderLeftColor :: Value -> B.Attribute
borderLeftColor = B.cssStyle "border-left-color" "border-left-color: " . B.stringValue . toString

borderTopStyle :: Value -> B.Attribute
borderTopStyle = B.cssStyle "border-top-style" "border-top-style: " . B.stringValue . toString

borderRightStyle :: Value -> B.Attribute
borderRightStyle = B.cssStyle "border-right-style" "border-right-style: " . B.stringValue . toString

borderBottomStyle :: Value -> B.Attribute
borderBottomStyle = B.cssStyle "border-bottom-style" "border-bottom-style: " . B.stringValue . toString

borderLeftStyle :: Value -> B.Attribute
borderLeftStyle = B.cssStyle "border-left-style" "border-left-style: " . B.stringValue . toString

borderTopWidth :: Value -> B.Attribute
borderTopWidth = B.cssStyle "border-top-width" "border-top-width: " . B.stringValue . toString

borderRightWidth :: Value -> B.Attribute
borderRightWidth = B.cssStyle "border-right-width" "border-right-width: " . B.stringValue . toString

borderBottomWidth :: Value -> B.Attribute
borderBottomWidth = B.cssStyle "border-bottom-width" "border-bottom-width: " . B.stringValue . toString

borderLeftWidth :: Value -> B.Attribute
borderLeftWidth = B.cssStyle "border-left-width" "border-left-width: " . B.stringValue . toString

borderWidth :: Value -> B.Attribute
borderWidth = B.cssStyle "border-width" "border-width: " . B.stringValue . toString

border :: Value -> B.Attribute
border = B.cssStyle "border" "border: " . B.stringValue . toString

captionSide :: Value -> B.Attribute
captionSide = B.cssStyle "caption-side" "caption-side: " . B.stringValue . toString

clear :: Value -> B.Attribute
clear = B.cssStyle "clear" "clear: " . B.stringValue . toString

clip :: Value -> B.Attribute
clip = B.cssStyle "clip" "clip: " . B.stringValue . toString

color :: Value -> B.Attribute
color = B.cssStyle "color" "color: " . B.stringValue . toString

cursor :: Value -> B.Attribute
cursor = B.cssStyle "cursor" "cursor: " . B.stringValue . toString

direction :: Value -> B.Attribute
direction = B.cssStyle "direction" "direction: " . B.stringValue . toString

display :: Value -> B.Attribute
display = B.cssStyle "display" "display: " . B.stringValue . toString

emptyCells :: Value -> B.Attribute
emptyCells = B.cssStyle "empty-cells" "empty-cells: " . B.stringValue . toString

float :: Value -> B.Attribute
float = B.cssStyle "float" "float: " . B.stringValue . toString

fontFamily :: Value -> B.Attribute
fontFamily = B.cssStyle "font-family" "font-family: " . B.stringValue . toString

fontSize :: Value -> B.Attribute
fontSize = B.cssStyle "font-size" "font-size: " . B.stringValue . toString

fontStyle :: Value -> B.Attribute
fontStyle = B.cssStyle "font-style" "font-style: " . B.stringValue . toString

fontVariant :: Value -> B.Attribute
fontVariant = B.cssStyle "font-variant" "font-variant: " . B.stringValue . toString

fontWeight :: Value -> B.Attribute
fontWeight = B.cssStyle "font-weight" "font-weight: " . B.stringValue . toString

height :: Value -> B.Attribute
height = B.cssStyle "height" "height: " . B.stringValue . toString

left :: Value -> B.Attribute
left = B.cssStyle "left" "left: " . B.stringValue . toString

letterSpacing :: Value -> B.Attribute
letterSpacing = B.cssStyle "letter-spacing" "letter-spacing: " . B.stringValue . toString

lineHeight :: Value -> B.Attribute
lineHeight = B.cssStyle "line-height" "line-height: " . B.stringValue . toString

listStyleImage :: Value -> B.Attribute
listStyleImage = B.cssStyle "list-style-image" "list-style-image: " . B.stringValue . toString

listStylePosition :: Value -> B.Attribute
listStylePosition = B.cssStyle "list-style-position" "list-style-position: " . B.stringValue . toString

listStyleType :: Value -> B.Attribute
listStyleType = B.cssStyle "list-style-type" "list-style-type: " . B.stringValue . toString

marginLeft :: Value -> B.Attribute
marginLeft = B.cssStyle "margin-left" "margin-left: " . B.stringValue . toString

marginRight :: Value -> B.Attribute
marginRight = B.cssStyle "margin-right" "margin-right: " . B.stringValue . toString

marginTop :: Value -> B.Attribute
marginTop = B.cssStyle "margin-top" "margin-top: " . B.stringValue . toString

marginBottom :: Value -> B.Attribute
marginBottom = B.cssStyle "margin-bottom" "margin-bottom: " . B.stringValue . toString

margin :: Value -> B.Attribute
margin = B.cssStyle "margin" "margin: " . B.stringValue . toString

maxHeight :: Value -> B.Attribute
maxHeight = B.cssStyle "max-height" "max-height: " . B.stringValue . toString

maxWidth :: Value -> B.Attribute
maxWidth = B.cssStyle "max-width" "max-width: " . B.stringValue . toString

minHeight :: Value -> B.Attribute
minHeight = B.cssStyle "min-height" "min-height: " . B.stringValue . toString

minWidth :: Value -> B.Attribute
minWidth = B.cssStyle "min-width" "min-width: " . B.stringValue . toString

orphans :: Value -> B.Attribute
orphans = B.cssStyle "orphans" "orphans: " . B.stringValue . toString

outlineColor :: Value -> B.Attribute
outlineColor = B.cssStyle "outline-color" "outline-color: " . B.stringValue . toString

outlineStyle :: Value -> B.Attribute
outlineStyle = B.cssStyle "outline-style" "outline-style: " . B.stringValue . toString

outlineWidth :: Value -> B.Attribute
outlineWidth = B.cssStyle "outline-width" "outline-width: " . B.stringValue . toString

outline :: Value -> B.Attribute
outline = B.cssStyle "outline" "outline: " . B.stringValue . toString

overflow :: Value -> B.Attribute
overflow = B.cssStyle "overflow" "overflow: " . B.stringValue . toString

paddingTop :: Value -> B.Attribute
paddingTop = B.cssStyle "padding-top" "padding-top: " . B.stringValue . toString

paddingRight :: Value -> B.Attribute
paddingRight = B.cssStyle "padding-right" "padding-right: " . B.stringValue . toString

paddingBottom :: Value -> B.Attribute
paddingBottom = B.cssStyle "padding-bottom" "padding-bottom: " . B.stringValue . toString

paddingLeft :: Value -> B.Attribute
paddingLeft = B.cssStyle "padding-left" "padding-left: " . B.stringValue . toString

padding :: Value -> B.Attribute
padding = B.cssStyle "padding" "padding: " . B.stringValue . toString

pageBreakAfter :: Value -> B.Attribute
pageBreakAfter = B.cssStyle "page-break-after" "page-break-after: " . B.stringValue . toString

pageBreakBefore :: Value -> B.Attribute
pageBreakBefore = B.cssStyle "page-break-before" "page-break-before: " . B.stringValue . toString

pageBreakInside :: Value -> B.Attribute
pageBreakInside = B.cssStyle "page-break-inside" "page-break-inside: " . B.stringValue . toString

position :: Value -> B.Attribute
position = B.cssStyle "position" "position: " . B.stringValue . toString

right :: Value -> B.Attribute
right = B.cssStyle "right" "right: " . B.stringValue . toString

tableLayout :: Value -> B.Attribute
tableLayout = B.cssStyle "table-layout" "table-layout: " . B.stringValue . toString

textAlign :: Value -> B.Attribute
textAlign = B.cssStyle "text-align" "text-align: " . B.stringValue . toString

textDecoration :: Value -> B.Attribute
textDecoration = B.cssStyle "text-decoration" "text-decoration: " . B.stringValue . toString

textIndent :: Value -> B.Attribute
textIndent = B.cssStyle "text-indent" "text-indent: " . B.stringValue . toString

textTransform :: Value -> B.Attribute
textTransform = B.cssStyle "text-transform" "text-transform: " . B.stringValue . toString

top :: Value -> B.Attribute
top = B.cssStyle "top" "top: " . B.stringValue . toString

unicodeBiDi :: Value -> B.Attribute
unicodeBiDi = B.cssStyle "unicode-bi-di" "unicode-bi-di: " . B.stringValue . toString

verticalAlign :: Value -> B.Attribute
verticalAlign = B.cssStyle "vertical-align" "vertical-align: " . B.stringValue . toString

visibility :: Value -> B.Attribute
visibility = B.cssStyle "visibility" "visibility: " . B.stringValue . toString

whitespace :: Value -> B.Attribute
whitespace = B.cssStyle "whitespace" "whitespace: " . B.stringValue . toString

width :: Value -> B.Attribute
width = B.cssStyle "width" "width: " . B.stringValue . toString

widows :: Value -> B.Attribute
widows = B.cssStyle "widows" "widows: " . B.stringValue . toString

wordSpacing :: Value -> B.Attribute
wordSpacing = B.cssStyle "word-spacing" "word-spacing: " . B.stringValue . toString

zIndex :: Value -> B.Attribute
zIndex = B.cssStyle "z-index" "z-index: " . B.stringValue . toString



{-# INLINE backgroundAttachment #-}
{-# INLINE backgroundColor #-}
{-# INLINE backgroundImage #-}
{-# INLINE backgroundPosition #-}
{-# INLINE backgroundRepeat #-}
{-# INLINE background #-}
{-# INLINE borderCollapse #-}
{-# INLINE borderColor #-}
{-# INLINE borderSpacing #-}
{-# INLINE borderStyle #-}
{-# INLINE borderTop #-}
{-# INLINE borderRight #-}
{-# INLINE borderBottom #-}
{-# INLINE borderLeft #-}
{-# INLINE borderTopColor #-}
{-# INLINE borderRightColor #-}
{-# INLINE borderBottomColor #-}
{-# INLINE borderLeftColor #-}
{-# INLINE borderTopStyle #-}
{-# INLINE borderRightStyle #-}
{-# INLINE borderBottomStyle #-}
{-# INLINE borderLeftStyle #-}
{-# INLINE borderTopWidth #-}
{-# INLINE borderRightWidth #-}
{-# INLINE borderBottomWidth #-}
{-# INLINE borderLeftWidth #-}
{-# INLINE borderWidth #-}
{-# INLINE border #-}
{-# INLINE captionSide #-}
{-# INLINE clear #-}
{-# INLINE clip #-}
{-# INLINE color #-}
{-# INLINE cursor #-}
{-# INLINE direction #-}
{-# INLINE display #-}
{-# INLINE emptyCells #-}
{-# INLINE float #-}
{-# INLINE fontFamily #-}
{-# INLINE fontSize #-}
{-# INLINE fontStyle #-}
{-# INLINE fontVariant #-}
{-# INLINE fontWeight #-}
{-# INLINE height #-}
{-# INLINE left #-}
{-# INLINE letterSpacing #-}
{-# INLINE lineHeight #-}
{-# INLINE listStyleImage #-}
{-# INLINE listStylePosition #-}
{-# INLINE listStyleType #-}
{-# INLINE marginLeft #-}
{-# INLINE marginRight #-}
{-# INLINE marginTop #-}
{-# INLINE marginBottom #-}
{-# INLINE margin #-}
{-# INLINE maxHeight #-}
{-# INLINE maxWidth #-}
{-# INLINE minHeight #-}
{-# INLINE minWidth #-}
{-# INLINE orphans #-}
{-# INLINE outlineColor #-}
{-# INLINE outlineStyle #-}
{-# INLINE outlineWidth #-}
{-# INLINE outline #-}
{-# INLINE overflow #-}
{-# INLINE paddingTop #-}
{-# INLINE paddingRight #-}
{-# INLINE paddingBottom #-}
{-# INLINE paddingLeft #-}
{-# INLINE padding #-}
{-# INLINE pageBreakAfter #-}
{-# INLINE pageBreakBefore #-}
{-# INLINE pageBreakInside #-}
{-# INLINE position #-}
{-# INLINE right #-}
{-# INLINE tableLayout #-}
{-# INLINE textAlign #-}
{-# INLINE textDecoration #-}
{-# INLINE textIndent #-}
{-# INLINE textTransform #-}
{-# INLINE top #-}
{-# INLINE unicodeBiDi #-}
{-# INLINE verticalAlign #-}
{-# INLINE visibility #-}
{-# INLINE whitespace #-}
{-# INLINE width #-}
{-# INLINE widows #-}
{-# INLINE wordSpacing #-}
{-# INLINE zIndex #-}


