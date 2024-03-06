

module Text.Blaze.Css21.Value
    ( Value(..)
    , toString
    , showCss
    )
where


import qualified    Prelude             as Prelude
import              Prelude             hiding ( Left , Right )
import              Data.Ratio          ( denominator , numerator )
import              Data.Word           ( Word8 )
import              Data.List           ( intersperse )
import              Numeric             ( showInt , showHex )

type Number = Rational

showNumber :: Number -> ShowS
showNumber a
    | denominator a == 1 = shows $ numerator a
    | otherwise          = shows $ fromRational a

showCss :: Value -> ShowS
showCss = (++) . to_string

toString :: Value -> String
toString = to_string

data Value
    = Values [Value]
    | Absolute
    | Always
    | Aqua
    | Armenian
    | Auto
    | Avoid 
    | Baseline
    | BiDiOverride
    | Black
    | Blink
    | Block
    | Blue
    | Bold
    | Bolder
    | Both
    | Bottom
    | Capitalize
    | Caption
    | Center
    | Circle
    | Cm Number
    | Collapse
    | Crosshair
    | Cursive
    | Dashed
    | Decimal
    | DecimalLeadingZero
    | Default
    | Disc
    | Dotted
    | Double
    | EResize
    | Em Number
    | Embed
    | Ex Number
    | FamilyName Prelude.String
    | Fantasy
    | Fixed
    | Fuchsia
    | Georgian
    | Gray
    | Green
    | Groove
    | Help
    | Hidden
    | Hide
    | Icon
    | In Number
    | Inherit
    | Inline
    | InlineBlock
    | InlineTable
    | Inset
    | Inside
    | Integer Prelude.Integer
    | Invert
    | Italic
    | Justify
    | Large
    | Larger
    | Left
    | Left_
    | Lighter
    | Lime
    | LineThrough
    | ListItem
    | LowerAlpha
    | LowerGreek
    | LowerLatin
    | LowerRoman
    | Lowercase
    | Ltr
    | Maroon
    | Medium
    | Menu
    | MessageBox
    | Middle
    | Mm Number
    | Monospace
    | Move
    | NResize
    | Navy
    | NeResize
    | NoRepeat
    | None
    | Normal
    | Nowrap
    | Number Rational
    | NwResize
    | Oblique
    | Olive
    | Orange
    | Outset
    | Outside
    | Overline
    | Pc Number
    | Percent Number
    | Percent100
    | Pointer
    | Pre
    | PreLine
    | PreWrap
    | Progress
    | Pt Number
    | Purple
    | Px Number
    | RGB Word8 Word8 Word8
    | RGB24 Word8 Word8 Word8
    | RGBPercent Rational Rational Rational
    | Rect Value Value Value Value
    | Red
    | Relative
    | Repeat
    | RepeatX
    | RepeatY
    | Ridge
    | Right
    | Right_
    | Rtl
    | SResize
    | SansSerif
    | Scroll
    | SeResize
    | Separate
    | Serif
    | Show
    | Silver
    | Small
    | SmallCaps
    | SmallCaption
    | Smaller
    | Solid
    | Square
    | Static
    | StatusBar
    | Sub
    | Super
    | SwResize
    | Table
    | TableCaption
    | TableCell
    | TableColumn
    | TableColumnGroup
    | TableFooterGroup
    | TableHeaderGroup
    | TableRow
    | TableRowGroup
    | Teal
    | Text
    | TextBottom
    | TextTop
    | Thick
    | Thin
    | Top
    | Transparent
    | URI Prelude.String
    | Underline
    | UpperAlpha 
    | UpperLatin
    | UpperRoman
    | Uppercase
    | Visible
    | WResize
    | Wait
    | Weight100
    | Weight200
    | Weight300
    | Weight400
    | Weight500
    | Weight600
    | Weight700
    | Weight800
    | Weight900 
    | White
    | XLarge
    | XSmall
    | XXLarge
    | XXSmall
    | Yellow
    | Zero
    deriving ( Show , Read )

to_string :: Value -> String
to_string (Values as)            = concat $ intersperse " " $ map to_string as
to_string Absolute               = "absolute"
to_string Always                 = "always"
to_string Aqua                   = "aqua"
to_string Armenian               = "armenian"
to_string Auto                   = "auto"
to_string Avoid                  = "avoid"
to_string Baseline               = "baseline"
to_string BiDiOverride           = "bi-di-override"
to_string Black                  = "black"
to_string Blink                  = "blink"
to_string Block                  = "block"
to_string Blue                   = "blue"
to_string Bold                   = "bold"
to_string Bolder                 = "bolder"
to_string Both                   = "both"
to_string Bottom                 = "bottom"
to_string Capitalize             = "capitalize"
to_string Caption                = "caption"
to_string Center                 = "center"
to_string Circle                 = "circle"
to_string (Cm a)                 = showNumber a "cm"
to_string Collapse               = "collapse"
to_string Crosshair              = "crosshair"
to_string Cursive                = "cursive"
to_string Dashed                 = "dashed"
to_string Decimal                = "decimal"
to_string DecimalLeadingZero     = "decimal-leading-zero"
to_string Default                = "default"
to_string Disc                   = "disc"
to_string Dotted                 = "dotted"
to_string Double                 = "double"
to_string EResize                = "e-resize"
to_string (Em a)                 = showNumber a "em"
to_string Embed                  = "embed"
to_string (Ex a)                 = showNumber a "ex"
to_string (FamilyName a)         = a
to_string Fantasy                = "fantasy"
to_string Fixed                  = "fixed"
to_string Fuchsia                = "fuchsia"
to_string Georgian               = "georgian"
to_string Gray                   = "gray"
to_string Green                  = "green"
to_string Groove                 = "groove"
to_string Help                   = "help"
to_string Hidden                 = "hidden"
to_string Hide                   = "hide"
to_string Icon                   = "icon"
to_string (In a)                 = showNumber a "in"
to_string Inherit                = "inherit"
to_string Inline                 = "inline"
to_string InlineBlock            = "inline-block"
to_string InlineTable            = "inline-table"
to_string Inset                  = "inset"
to_string Inside                 = "inside"
to_string (Integer a)            = show a
to_string Invert                 = "invert"
to_string Italic                 = "italic"
to_string Justify                = "justify"
to_string Large                  = "large"
to_string Larger                 = "larger"
to_string Left                   = "left"
to_string Left_                  = "left"
to_string Lighter                = "lighter"
to_string Lime                   = "lime"
to_string LineThrough            = "line-through"
to_string ListItem               = "list-item"
to_string LowerAlpha             = "lower-alpha"
to_string LowerGreek             = "lower-greek"
to_string LowerLatin             = "lower-latin"
to_string LowerRoman             = "lower-roman"
to_string Lowercase              = "lowercase"
to_string Ltr                    = "ltr"
to_string Maroon                 = "maroon"
to_string Medium                 = "medium"
to_string Menu                   = "menu"
to_string MessageBox             = "message-box"
to_string Middle                 = "middle"
to_string (Mm a)                 = showNumber a "mm"
to_string Monospace              = "monospace"
to_string Move                   = "move"
to_string NResize                = "n-resize"
to_string Navy                   = "navy"
to_string NeResize               = "ne-resize"
to_string NoRepeat               = "no-repeat"
to_string None                   = "none"
to_string Normal                 = "normal"
to_string Nowrap                 = "nowrap"
to_string (Number a)             = show $ fromRational a
to_string NwResize               = "nw-resize"
to_string Oblique                = "oblique"
to_string Olive                  = "olive"
to_string Orange                 = "orange"
to_string Outset                 = "outset"
to_string Outside                = "outside"
to_string Overline               = "overline"
to_string (Pc a)                 = showNumber a "pc"
to_string (Percent a)            = showNumber a "%"
to_string Percent100             = "100%"
to_string Pointer                = "pointer"
to_string Pre                    = "pre"
to_string PreLine                = "pre-line"
to_string PreWrap                = "pre-wrap"
to_string Progress               = "progress"
to_string (Pt a)                 = showNumber a "pt"
to_string Purple                 = "purple"
to_string (Px a)                 = showNumber a "px"
to_string (RGB r g b)            = "rgb(" ++ showInt r "," ++ showInt g "," ++ showInt b ")"
to_string (RGB24 r g b)          = "rgb(#" ++ showHex r ",#" ++ showHex g ",#" ++ showHex b ")"
to_string (RGBPercent r g b)     = "rgb(" ++ showNumber r "%," ++ showNumber g "%," ++ showNumber b "%)"
to_string (Rect t r b l)         = "rect(" ++ showCss t "," ++ showCss r "," ++ showCss b "," ++ showCss l ")"
to_string Red                    = "red"
to_string Relative               = "relative"
to_string Repeat                 = "repeat"
to_string RepeatX                = "repeat-x"
to_string RepeatY                = "repeat-y"
to_string Ridge                  = "ridge"
to_string Right                  = "right"
to_string Right_                 = "right"
to_string Rtl                    = "rtl"
to_string SResize                = "s-resize"
to_string SansSerif              = "sans-serif"
to_string Scroll                 = "scroll"
to_string SeResize               = "se-resize"
to_string Separate               = "separate"
to_string Serif                  = "serif"
to_string Show                   = "show"
to_string Silver                 = "silver"
to_string Small                  = "small"
to_string SmallCaps              = "small-caps"
to_string SmallCaption           = "small-caption"
to_string Smaller                = "smaller"
to_string Solid                  = "solid"
to_string Square                 = "square"
to_string Static                 = "static"
to_string StatusBar              = "status-bar"
to_string Sub                    = "sub"
to_string Super                  = "super"
to_string SwResize               = "sw-resize"
to_string Table                  = "table"
to_string TableCaption           = "table-caption"
to_string TableCell              = "table-cell"
to_string TableColumn            = "table-column"
to_string TableColumnGroup       = "table-column-group"
to_string TableFooterGroup       = "table-footer-group"
to_string TableHeaderGroup       = "table-header-group"
to_string TableRow               = "table-row"
to_string TableRowGroup          = "table-row-group"
to_string Teal                   = "teal"
to_string Text                   = "text"
to_string TextBottom             = "text-bottom"
to_string TextTop                = "text-top"
to_string Thick                  = "thick"
to_string Thin                   = "thin"
to_string Top                    = "top"
to_string Transparent            = "transparent"
to_string (URI a)                = "uri('" ++ a ++ "')"
to_string Underline              = "underline"
to_string UpperAlpha             = "upper-alpha"
to_string UpperLatin             = "upper-latin"
to_string UpperRoman             = "upper-roman"
to_string Uppercase              = "uppercase"
to_string Visible                = "visible"
to_string WResize                = "w-resize"
to_string Wait                   = "wait"
to_string Weight100              = "weight100"
to_string Weight200              = "weight200"
to_string Weight300              = "weight300"
to_string Weight400              = "weight400"
to_string Weight500              = "weight500"
to_string Weight600              = "weight600"
to_string Weight700              = "weight700"
to_string Weight800              = "weight800"
to_string Weight900              = "weight900"
to_string White                  = "white"
to_string XLarge                 = "x-large"
to_string XSmall                 = "x-small"
to_string XXLarge                = "x-x-large"
to_string XXSmall                = "x-x-small"
to_string Yellow                 = "yellow"
to_string Zero                   = "0"

