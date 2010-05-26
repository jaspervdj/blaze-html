BlazeHtml RFC
=============

Introduction
------------

BlazeHtml started out on ZuriHac 2010. Now, Jasper Van der Jeugt is working on
it as a student to Google Summer of Code for haskell.org. His mentors are Simon
Meier and Johan Tibell. The goal is to create a high-performance HTML generation
library.

In the past few weeks, we have been exploring the performance and design of
different drafts of this library. Now, the time has come to ask some questions
to the Haskell community -- more specifically the future users of BlazeHtml as
well as current users of other HTML generation libraries.


About this file
---------------

This document is a literate Haskell file. It serves two purposes: (1) it
explains our current ideas and (2) it asks you, as the reader, for feedback. If
you want to run this file or experiment with the code, you need to check out the
code from github:

    git clone git://github.com/jaspervdj/BlazeHtml.git

Enter the newly created directory `BlazeHtml` using

    cd BlazeHtml

and load this document using

    ghci doc/RFC.lhs

Note that we placed a `.ghci` file in the `BlazeHtml` directory. It sets the
correct include directories for ghci.


Notational preliminaries
------------------------

A "string" is a sequence of Unicode codepoints. A value of type `String` is a
concrete representation of a string; i.e. a Haskell list of Unicode
codepoints. A value of type `Text` is another concrete representation of a
string provided by the  `Data.Text` library. "Encoding" a string means
converting the sequence of Unicode codepoints to a sequence of bytes, using a
format like UTF-8 or UTF-16.

A "HTML document" is a tree whose nodes are HTML elements with string-valued
attributes and whose leaves are strings. "Rendering" a HTML document means
converting it to a string that will result in the same HTML tree when parsed by
a HTML parser.


Problem definition
------------------

The goal of the BlazeHtml project is to create a light-weight Haskell combinator
language for HTML documents that can be rendered as efficiently as possible.


Supported string representations
--------------------------------

Obviously, we need to fix the concrete string representations to be used for
describing  attributes and leaves of HTML documents. We have chosen to support
both `String` values as  well as `Text` values, as we assume that these are the
most common representations for strings  occuring in user code.

*Q1*: Are there other string representations that should be supported natively;
i.e. without converting them to `Data.Text` or `String` first?

Note that we enable the  `OverloadedStrings` language extension to also support
string literals of type `Text`.

> {-# LANGUAGE OverloadedStrings #-}


Modules
-------

We import the Prelude hiding some functions, to avoid clashes: `head`, `id` and
`div` are all HTML elements. Since we do not use the corresponding Prelude
functions in our program, we will just hide them instead of qualifying either
the Prelude or our modules.

> import Prelude hiding (head, id, div)

There are different HTML standards. For example,

- XHTML
- HTML 4 Strict
- HTML 4 Transitional
- HTML 4 Frameset
- HTML 5

*Q2*: What HTML standards should the library *at least* support? 

*Q3*: Which HTML version would you preferably use?

Currently, we decided to use the HTML 4 Strict standard, as it seems to be 
the most used one.

Our goal is that a description of a Html document using BlazeHtml looks as
similar as possible to real HTML -- and, if possible, even easier on the eyes.
Hence, we want to provide for every HTML element and attribute a combinator with
exactly the same name. However, this is not possible due to two reasons: (1)
There are HTML element and attribute names that conflict with Haskell keywords,
or Haskell naming conventions. (2) There are HTML elements having the same name
as HTML attributes.

To solve the first problem, we adopt the convention that the combinator for a
HTML element (or an attribute) that conflicts with a Haskell keyword (like
class)  is suffixed with an underscore (i.e class_ instead of class). Attributes
like `http-equiv` (Haskell doesn't like the '-' character) will be written as
`http_equiv`.

To solve the second problem, we split the combinators for elements and
attributes into separate modules. This way the library user can decide on how to
handle the conflicting names using hiding and/or qualified imports; e.g.  we
could qualify the attributes such that the 'title' attribute combinator becomes
'A.title'.

*Q4*: What do you think of this approach for chosing combinator names? 

Several HTML elements conflict with the Prelude; e.g. `head` or `map`. We are
not sure how to resolve these clashes. Currently, we leave it up to the library
user to use appropriate hiding and qualifying. This works fine, if the library
user separates the busines logic from the presentation layer, and thus puts
BlazeHtml templates in separate modules, where little logic is required. Another
way is to also regard functions in the Prelude (or a bigger fixed set of
libraries) as "Haskell keywords" and use underscore suffixing for name-conflict
resolution.

*Q5*: Would you also regard the Prelude (or a bigger set of libraries) as fixed 
"Haskell keywords" and use underscore suffixing for conflict resolution?

Currently, we decided that all our modules will share the `Text.Blaze` prefix.

> import Text.Blaze.Html4.Strict hiding (map)
> import Text.Blaze.Html4.Strict.Attributes hiding (title)

*Q6*: Do you think `Text.Blaze.X` is a proper name for a module? Or should we
drop `Blaze` and use `Text.Html` instead? 

An advantage of using the `Text.Html` prefix is that the user can directly see
what the module is meant for. A disadvantage is that the likelihood of module
clashes on Hackages increases.

Two more imports to satisfy the compiler:

> import Data.Monoid (mconcat)
> import Control.Monad (forM_)

As you will see later, we will render our Html documents to UTF-8 encoded
ByteStrings. For displaying these, we also need `putStrLn` from
`Data.ByteString.Lazy`.

> import qualified Data.ByteString.Lazy as LB


Syntax
------

We will demonstrate our combinator langugage by example. This is the
(simple) HTML document we want to produce:

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

In BlazeHtml, we (ab)use do-notation to get a very light-weight syntax; i.e.
monadic sequencing is used to represent concatenation of Html documents.

> page1 = html $ do
>     head $ do
>         title "Introduction page."
>         link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
>     body $ do
>         div ! id "header" $ "Syntax"
>         p "This is an example of BlazeHtml syntax."
>         ul $ forM_ [1, 2, 3] (li . string . show)

This use has its cost, as we don't support passing values inside the monad.
Hence, `return x >>= f != f x`. We tried supporting passing values, but it cost
too much performance. 

The correct way out would be to drop this instance and have the user use the
functions working on Monoids directly, as in the following example describing
the same page:

> page2 = html $ mconcat
>     [ head $ mconcat
>         [ title "Introduction page."
>         , link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
>         ]
>     , body $ mconcat
>         [ div ! id "header" $ "Syntax"
>         , p "This is an example of BlazeHtml syntax."
>         , ul $ mconcat $ map (li . string . show) [1, 2, 3]
>         ]
>     ]

The syntax choice is up to the end user. We tend to prefer the first notation,
as we think it is more light-weight.

The main function just outputs the two pages:

> main = do
>     LB.putStrLn $ renderHtml page1
>     LB.putStrLn $ renderHtml page2

*Q7*: How do you think about this syntax, generally?

*Q8*: Do you think `!` is a good operator for setting attributes? 

We made an initial choice for `!` because the old HTML package uses this.
However, this operator looks more like array indexing. It is not too late to
change this, suggestions are very welcome.

*Q9*: How should multiple attributes be handled? 

In the above example, we used  the `!` again for the next attribute:

    link ! rel "stylesheet" ! type "text/css" ! href "screen.css"

Another option would be to define a variant that takes a list of attributes:

    link !> [rel "stylesheet", type "text/css", href "screen.css"]

Or, we could use a type class to give the `!` different uses, and thus have:

    link ! [rel "stylesheet", type "text/css", href "screen.css"]

The last option will, however, introduce a more complicated type for attributes,
more complicated type errors, and a performance overhead in some cases.


Rendering & encoding
--------------------

As said before, BlazeHtml supports strings represented either as `String` or
`Data.Text` values. These two types support all Unicode codepoints, so the
encoding format should support all Unicode codepoints, too. If the encoding
format does not support all Unicode codepoints, then rendering and encoding
cannot be separated nicely because unencodable characters must already be
escaped accordingly during rendering.

We think that more than 95% of the end users won't need support for lossy
encodings. Hence, we choose not to support them. Note that all desktop browsers,
and most mobile browsers support superior encodings.

*Q10*: Do you need support for "lossy" encodings, e.g. Latin-1? If yes, could you
describe your use case more precisely?

Fixing the encoding statically greatly helps for achieving the best possible
performance. Hence, we fix the encoding of rendered HTML documents to UTF-8
because this is the most used encoding for HTML documents [1].

*Q11*: What other encodings do you need support for?

Note that for a non-performance-critical code path you can always decode and
re-encode the rendered and UTF-8 encoded HTML document.


Speed
-----

Possibly, you think that the best representation for a rendered HTML document
is a `Text` value. Then, one could use the functions from `Data.Text` to encode
this `Text` value to the desired final encoding. However, this conflicts with
the goal of being as efficient as possible. For maximal efficiency, one wants to
spend as little work as possible for each byte that is output. 

Hence, if we convert our data directly to the final encoding, then we save
one intermediate representation. In our current prototype implementation, this
is reflected by the fact that we build the UTF-8 encoded sequence of bytes
directly using a slightly modified version of the `Builder` monoid from
`Data.Binary`. This has the nice side-effect that the `Lazy.ByteString`
generated by the `Builder` monoid consists of a list of big (32kb) chunks that
can be sent over the network efficiently using the `network-bytestring` [2]
library.

Our preliminary benchmark suite shows that this is a very promising
approach. You can run these benchmarks by calling

    make bench-html

In the `BlazeHtml` directory that you created in the beginning of this RFC.

Note that these benchmarks also contain the "BigTable" benchmark that is
implemented in many different templating engines. It measures the rendering time
of a big <table> that has 1000 rows and 10 columns, and every row has the simple
content 1, 2, 3, ... 10. Our prototype library is much faster than other
templating engines such as Spitfire, ClearSilver, ERB and Erubis. More
information can be found in this
blogpost [3].

*Q12*: Do you know of other libraries or benchmarks that we should compare to?


Epilogue
--------

Most modern web applications embrace the MVC design pattern. In this pattern,
BlazeHtml is part of the "View". Two other components are needed -- the "Model"
(data retrieval & persistence) and the "Controller" (the server).

*Q13*: What other libraries would you use BlazeHtml with? 

*Q14*: Do you see any problems with respect to integrating BlazeHtml in 
your favourite web-framework/server?


[1]: http://googleblog.blogspot.com/2008/05/moving-to-unicode-51.html
[2]: http://hackage.haskell.org/package/network-bytestring
[3]: http://jaspervdj.be/posts/2010-04-28-blazehtml-initial-results.html


Looking forward to your feedback
--------------------------------

Jasper van der Jeugt and Simon Meier
