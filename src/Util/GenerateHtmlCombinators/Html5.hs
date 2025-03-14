module Util.GenerateHtmlCombinators.Html5
    ( parents
    , leafs
    , attributes
    ) where

parents :: [String]
parents =
    [ "a"
    , "abbr"
    , "address"
    , "article"
    , "aside"
    , "audio"
    , "b"
    , "bdi"
    , "bdo"
    , "blockquote"
    , "body"
    , "button"
    , "canvas"
    , "caption"
    , "cite"
    , "code"
    , "colgroup"
    , "command"
    , "data"
    , "datalist"
    , "dd"
    , "del"
    , "details"
    , "dfn"
    , "dialog"
    , "div"
    , "dl"
    , "dt"
    , "em"
    , "fieldset"
    , "figcaption"
    , "figure"
    , "footer"
    , "form"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "head"
    , "header"
    , "hgroup"
    , "html"
    , "i"
    , "iframe"
    , "ins"
    , "kbd"
    , "label"
    , "legend"
    , "li"
    , "main"
    , "map"
    , "mark"
    , "menu"
    , "meter"
    , "nav"
    , "noscript"
    , "object"
    , "ol"
    , "optgroup"
    , "option"
    , "output"
    , "p"
    , "picture"
    , "pre"
    , "progress"
    , "q"
    , "rp"
    , "rt"
    , "ruby"
    , "s"
    , "samp"
    , "search"
    , "script"
    , "section"
    , "select"
    , "slot"
    , "small"
    , "span"
    , "strong"
    , "style"
    , "sub"
    , "svg"
    , "summary"
    , "sup"
    , "table"
    , "tbody"
    , "td"
    , "template"
    , "text"
    , "textarea"
    , "textpath"
    , "tfoot"
    , "th"
    , "thead"
    , "time"
    , "title"
    , "tr"
    , "tspan"
    , "u"
    , "ul"
    , "var"
    , "video"
    ]

leafs :: [String]
leafs =
    [ "area"
    , "base"
    , "br"
    , "circle"
    , "col"
    , "ellipse"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "keygen"
    , "line"
    , "link"
    , "menuitem"
    , "meta"
    , "param"
    , "polygon"
    , "polyline"
    , "rect"
    , "source"
    , "track"
    , "wbr"
    ]

attributes :: [String]
attributes =
    [ "accept"
    , "accept-charset"
    , "accesskey"
    , "action"
    , "alt"
    , "async"
    , "autocomplete"
    , "autofocus"
    , "autoplay"
    , "challenge"
    , "charset"
    , "checked"
    , "cite"
    , "class"
    , "cols"
    , "colspan"
    , "content"
    , "contenteditable"
    , "contextmenu"
    , "controls"
    , "coords"
    , "cx"
    , "cy"
    , "data"
    , "datetime"
    , "defer"
    , "dir"
    , "disabled"
    , "download"
    , "draggable"
    , "dx"
    , "dy"
    , "enctype"
    , "for"
    , "form"
    , "formaction"
    , "formenctype"
    , "formmethod"
    , "formnovalidate"
    , "formtarget"
    , "headers"
    , "height"
    , "hidden"
    , "high"
    , "href"
    , "hreflang"
    , "http-equiv"
    , "icon"
    , "id"
    , "ismap"
    , "item"
    , "itemprop"
    , "itemscope"
    , "itemtype"
    , "keytype"
    , "label"
    , "lang"
    , "lengthadjust"
    , "list"
    , "loop"
    , "low"
    , "manifest"
    , "max"
    , "maxlength"
    , "media"
    , "method"
    , "min"
    , "minlength"
    , "multiple"
    , "muted"
    , "name"
    , "novalidate"
    , "onbeforeonload"
    , "onbeforeprint"
    , "onblur"
    , "oncanplay"
    , "oncanplaythrough"
    , "onchange"
    , "oncontextmenu"
    , "onclick"
    , "ondblclick"
    , "ondrag"
    , "ondragend"
    , "ondragenter"
    , "ondragleave"
    , "ondragover"
    , "ondragstart"
    , "ondrop"
    , "ondurationchange"
    , "onemptied"
    , "onended"
    , "onerror"
    , "onfocus"
    , "onformchange"
    , "onforminput"
    , "onhaschange"
    , "oninput"
    , "oninvalid"
    , "onkeydown"
    , "onkeypress"
    , "onkeyup"
    , "onload"
    , "onloadeddata"
    , "onloadedmetadata"
    , "onloadstart"
    , "onmessage"
    , "onmousedown"
    , "onmousemove"
    , "onmouseout"
    , "onmouseover"
    , "onmouseup"
    , "onmousewheel"
    , "ononline"
    , "onpagehide"
    , "onpageshow"
    , "onpause"
    , "onplay"
    , "onplaying"
    , "onprogress"
    , "onpropstate"
    , "onratechange"
    , "onreadystatechange"
    , "onredo"
    , "onresize"
    , "onscroll"
    , "onseeked"
    , "onseeking"
    , "onselect"
    , "onstalled"
    , "onstorage"
    , "onsubmit"
    , "onsuspend"
    , "ontimeupdate"
    , "onundo"
    , "onunload"
    , "onvolumechange"
    , "onwaiting"
    , "open"
    , "optimum"
    , "pathlength"
    , "pattern"
    , "ping"
    , "placeholder"
    , "points"
    , "poster"
    , "preload"
    , "property"
    , "pubdate"
    , "radiogroup"
    , "readonly"
    , "r"
    , "rel"
    , "required"
    , "reversed"
    , "role"
    , "rotate"
    , "rows"
    , "rowspan"
    , "rx"
    , "ry"
    , "sandbox"
    , "scope"
    , "scoped"
    , "seamless"
    , "selected"
    , "shape"
    , "size"
    , "sizes"
    , "spacing"
    , "span"
    , "spellcheck"
    , "src"
    , "srcdoc"
    , "start"
    , "startoffset"
    , "step"
    , "style"
    , "subject"
    , "summary"
    , "tabindex"
    , "target"
    , "textlength"
    , "title"
    , "type"
    , "usemap"
    , "value"
    , "viewbox"
    , "width"
    , "wrap"
    , "xmlns"
    , "x"
    , "y"
    , "x1"
    , "x2"
    , "y1"
    , "y2"
    ]
