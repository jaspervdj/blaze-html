BlazeHtml RFC
=============

Introduction
------------

I was accepted as a student to Google Summer of Code for haskell.org. My task
is now to create a high-performance HTML generation library. We have chosen to
create a combinator library with a light-weight syntax called BlazeHtml.

In the past few weeks, we have been exploring the performance and design of
different drafts of this library. Now, the time has come to ask some questions
to the Haskell community -- more specifically the future users of BlazeHtml as
well as current users of other HTML generation libraries.

About this file
---------------

This document is a literate Haskell file. It serves two purposes: (1) it
explains our current ideas and (2) it asks you, as the reader, for feedback.
If you want to run this file or experiment with the code, you need to check out
the code from github:

    git clone git://github.com/jaspervdj/BlazeHtml.git

Enter the newly created directory `BlazeHtml` using

    cd BlazeHtml

and load this document using

    ghci doc/RFC.lhs

Note that we placed a `.ghci` file in the `BlazeHtml` directory. It sets the
correct include directories for ghci.

Input strings
-------------

With "input", we mean the string values the end user annotes the HTML with --
for example: HTML content and HTML attribute values.

BlazeHtml supports both input from `Data.Text` and `String`.

We are very likely to choose *not* to support input from `ByteString`s, or at
least discourage this. The reason behind this is that if a function receives a
`ByteString`, there is no way to correctly infer the encoding in all cases.

*Q1*: Do you known other input types that you need to support natively; i.e.
without converting them to Data.Text or String first?

We want to use static strings in our program of both type `Data.Text` as well as
`String`. For this purpose, we require the `OverloadedStrings` language
extension:

> {-# LANGUAGE OverloadedStrings #-}

Modules
-------

I am going to import Prelude hiding some functions, to avoid clashes: `head`,
`id` and `div` are all HTML elements. Since we do not use the corresponding
Prelude functions in our program, we will just hide them instead of qualifying
either the Prelude or our modules.

We also use the `putStrLn` from `Data.ByteString.Lazy` instead of the one from
the Prelude.

> import Prelude hiding (head, id, div, putStrLn)
> import Data.ByteString.Lazy (putStrLn)

The BlazeHtml main module would be `Text.Blaze`. In this module, we define
general functions one needs when using BlazeHtml, for example, the operator to
set attributes.

[[SM: Why don't we give the user everything he needs when he imports 
      Text.Html4 and Text.Html4.Attributes ?

      The fewer things I have to import the better.
]]

*Q2*: Do you think `Text.Blaze` is a proper name for the main module? Or should
we drop `Blaze` and use `Text.Html` instead?

> import Text.Blaze

Different versions of HTML are available. To sum up a few:

- XHTML
- HTML 4 Strict
- HTML 4 Transitional
- HTML 4 Frameset
- HTML 5
- ...

*Q3*: What versions and variants should the library *at least* support? Which
HTML version would you preferably use?

We decide on a standard to use -- let's take HTML 4 Strict, for example.

Our goal is that a description of a Html document using BlazeHtml looks as
similar as possible to real HTML -- and, if possible, easier to the eyes. Hence,
we want to provide for every HTML element and attribute a combinator with
exactly the same name. However, this is not possible due to two reasons: (1)
There are HTML element and attribute names that conflict with Haskell keywords,
or Haskell naming conventions. (2) There are HTML elements having the same name
as HTML attributes.

To solve the first problem, we adopt the convention that the combinator for a
HTML element (or an attribute) that conflicts with a Haskell keyword (like class) 
is suffixed with and underscore (i.e class_ instead of class). Attributes like
`http-equiv` (Haskell doesn't like the '-' character) will be written as
`http_equiv`.

To solve the second problem, we split the combinators for elements and
attributes into separate modules. This way the library user can decide on how
to handle the conflicting names using hiding and/or qualified imports; e.g.  we
could qualify the attributes such that the 'title' combinator becomes
'A.title'.

A lot of functions clash with the prelude as well (e.g. `head`, `map`). We do
not think this will be a big problem, since a web developer probably wants to
split their busines logic from the presentation layer, and thus putting the
BlazeHtml templates in separate modules, where very little logic is required.

*Q4*: What do you think of this approach on modules/combinator names?

> import Text.Blaze.Html4.Strict hiding (map)
> import Text.Blaze.Html4.Strict.Attributes hiding (title)

Two more imports to satisfy the compiler:

> import Data.Monoid (mconcat)
> import Control.Monad (forM_)

Syntax
------

We're going to write an HTML page now. This is the (simple) page we want to
produce:

    <html>
        <head>
            <title>Introduction page.</title>
            <link href=\"screen.css\" type=\"text/css\" rel=\"stylesheet\" />
        </head>
        <body>
            <div id=\"header\">Syntax</div>
            <p>
                This is an example of BlazeHtml syntax.
            </p>
            <ul>
                <li>1</li>
                <li>2</li>
                <li>3</li>
            </ul>
        </body>
    </html>

In BlazeHtml, we abuse do-notation to get a very light syntax. Essentially, we abuse 
monadic sequencing to represent concatenation of Html documents.

> page1 = html $ do
>     head $ do
>         title "Introduction page."
>         link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
>     body $ do
>         div ! id "header" $ "Syntax"
>         p "This is an example of BlazeHtml syntax."
>         ul $ forM_ [1, 2, 3] (li . string . show)

This abuse has its cost, as we don't support passing values inside the monad.
Hence, `return x >>= f != f x`. We tried supporting passing values, but it
cost too much performance. 

The correct way out would be to drop this instance and have the user use the
functions working on Monoids directly. As in the following example describing
the same page.

> page2 = html $ mconcat
>     [ head $ mconcat
>         [ title "Introduction page."
>         , link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
>         ]
>     , body $ mconcat
>         [ div ! id "header" $ "Syntax"
>         , p "This is an example of BlazeHtml syntax."
>         , ul $ mconcat $ map (li . string . show) [1, 2, 3]
>         ]
>     ]

The syntax choice is then up to the end user. We tend to prefer the first
notation, we think it is easier to the eyes because there is almost no operator
noise.

*Q5*: How do you think about this syntax, generally?

*Q6*: Do you think `!` is a good operator for setting attributes? We made an
initial choice for `!` because the old HTML package uses this. However, this 
operator looks more like array indexing. It is not too late to change this, 
suggestions are very welcome.

*Q7*: How should multiple attributes be handled? In the above example, we used 
the `!` again for the next attribute:

    link ! rel "stylesheet" ! type "text/css" ! href "screen.css"

Another option would be to define a variant that takes a list of attributes:

    link !> [rel "stylesheet", type "text/css", href "screen.css"]

Or, we could use a type class to give the `!` different uses, and thus have:

    link ! [rel "stylesheet", type "text/css", href "screen.css"]

The last option will, however, introduce a more complicated type for attributes,
more complicated type errors, and some performance overhead in some cases.


Rendering & encoding
--------------------

We make a distinction in terminology between rendering and encoding here. The
process of mapping the tree of HTML elements annotated with attributes and
content to its representation as a sequence of Unicode chreacters according
to a specific rendering format.

Encoding means mapping a sequence of Unicode characters to its representation
as sequence of bytes according to a specific encoding format.

As said before, we will support input from both `String` and `Data.Text`
datatypes. These two datatypes support all Unicode codepoints, so the encoding
format should support all Unicode codepoints, too. If the encoding format does
not support all Unicode codepoints, then rendering and encoding cannot be
separated nicely because unencodable characters must already be escaped 
accordingly during rendering. 

We think that more than 90% of the end users just need UTF-8 encoded HTML
output, but we could, of course, be wrong.

*Q9*: Do you need support for "lossy" encodings, e.g. Latin-1? If yes, could you
describe your use case more precisely?

Note that all desktop browsers, and most mobile browsers support superior
encodings like UTF-8.

Also, we would like to pose some questions about the output format. There seem
to be two major options:

- Output to an encoded lazy ByteString representing an UTF-8 encoded sequence
  of Unicode characters. The advantage of this scenario is that the result can
  be sent over the network directly and efficiently, for example
  with the network-bytestring[1] library. We think (according to our
  assumptions) that in the end, a user will need the UTF-8 encoded sequence of
  Unicode characters.

- Output to a Text value. The advantage here is that we have a nicer separation
  of concerns (encoding would be separated from HTML generation).

The first option is definitely faster.

*Q10*: Should we sacrifice some speed to have the second option as a possibility?

Currently, the type for HTML snippets in Haskell is simple `Html`. If we would
make `Html` a type class or use a user-defined fold over the tree, it would be
possible to render the same `Html` value to different encodings.

*Q11*: Do you need to be able to render a certain snippet in different
encodings?

When the web server sends out an HTML document, the browser will need to know
the encoding of this document, so it can be interpreted correctly. The most
common way to do this is to send `Content-Type` in the HTTP header.

Based on our assumptions, we think that BlazeHtml will be used with a Haskell
web server or framework, such as HappStack, Hyena, Yesod or Snap. These web
servers can send the encoding correctly.

However, when the user has no control over the web server, the encoding might
be sent incorrectly. In such a case, we need to use a
`<meta http-equiv="..." content="..." />` tag.  If we want to support the second
option in BlazeHtml, specifying a document in an encoding-independent method
becomes harder (but not impossible, we wrote a prototype implementation that
supports this as well).

*Q12*: Do you have a use case in which the server cannot send the encoding
correctly?

> main = do
>     putStrLn $ renderHtml page1
>     putStrLn $ renderHtml page2


Speed
-----

There is a preliminary suite of benchmarks available. We focused on the
"BigTable" mostly, this is a very simple microbenchmark, implemented in many
different templating engines. It times the rendering of a big <table>. This
table has 1000 rows and 10 columns, and every row has the simple content 1, 2,
3, ... 10.

You can run our benchmarks like this:

    make bench-html

Our prototype library is much faster than other templating engines such as
Spitfire, ClearSilver, ERB and Erubis. More information can be found in this
blogpost[2].


Epilogue
--------

Most modern web applications embrace the MVC design pattern. In this pattern,
BlazeHtml could be considered the "View". Two other components are needed --
the "Model" (data retrieval & persistence) and the "Controller" (the server).

*Q13*: What other libraries would you use BlazeHtml with? We could provide
integration where needed, or at least try to make integration as simple as
possible.

[1]: http://hackage.haskell.org/package/network-bytestring
[2]: http://jaspervdj.be/posts/2010-04-28-blazehtml-initial-results.html
