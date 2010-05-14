BlazeHtml RFC
=============

Introduction
------------

I was accepted as a student to Google Summer of Code for haskell.org. My task
is now to create a high-performance HTML generation library. We have chosen to
create a combinator library with a light-weight syntax called BlazeHtml.

In the past few weeks, we have been exploring the performance and design of
different drafts of this library. Now, the time has come to ask some questions
to the Haskell community -- and more specifically, the end users of the library,
and current users of other HTML generation libraries.

About this file
---------------

This document is a literate Haskell file in which we explain our current ideas
and, if possible, request feedback. If you want to run this file or experiment
with the code, you need to check out the code from github:

    git clone git://github.com/jaspervdj/BlazeHtml.git

Enter the directory. You can then run the benchmarks like this:

    cd BlazeHtml
    make bench-html

The folder also contains a `.ghci` file which will set the correct include
directories for ghci. To load this document, use:

    ghci doc/RFC.lhs

Input strings
-------------

BlazeHtml is mostly written to support `Data.Text` values as "input": it takes
`Data.Text` values as parameters for attributes, as content for text elements...

We also support `String` as input types, but we tend to this discourage this.

*QX*: What do you think of these input types?

We would like to avoid using `pack` everywhere in our code, so we use the
`OverloadedStrings` language extension:

> {-# LANGUAGE OverloadedStrings #-}

Modules
-------

I am going to import Prelude hiding some functions, to avoid clashes.

> import Prelude hiding (head, id, div, putStrLn)
> import Data.ByteString.Lazy (putStrLn)

The BlazeHtml main module would be `Text.Blaze`. In this module, we define
general functions one needs when using BlazeHtml, for example, the operator to
set attributes.

*QX*: Do you think `Text.Blaze` is a proper name for the main module? Or should
we drop `Blaze` and use `Text.Html` instead?

> import Text.Blaze

Different versions of HTML are available. To sum up a few:

- XHTML
- HTML 4 Strict
- HTML 4 Transitional
- HTML 4 Frameset
- HTML 5
- ...

*QX*: My first question is: what versions and variants should the library *at
least* support? Which HTML version would you preferably use?

We decide on a standard to use -- let's take HTML 4 Strict, because it is the
only standard currently supported in the BlazeHtml prototype.

When we consider the set of HTML attributes and the set of HTML tag names, there
is an intersection which includes, for example "style".

This would require us to have two variations on the "style" combinator -- one
producing an attribute, and one producing an element. What we could do here is,
for example, append an apostroph to the attribute combinators.

However, we propose a different solution. After some discussions, it seemed
best to have two modules for a HTML set.

*QX*: What do you think of this approach?

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

BlazeHtml is going to use do-notation syntax by declaring a monad instance. This
is the above example written in BlazeHtml with do-notation:

> page1 = html $ do
>     head $ do
>         title "Introduction page."
>         link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
>     body $ do
>         div ! id "header" $ "Syntax"
>         p "This is an example of BlazeHtml syntax."
>         ul $ forM_ [1, 2, 3] (li . string . show)

Take note: we use a monadic notation only for the do-syntax. The `>>=` operator
is defined but has no practical use -- we can use convenient functions like
`forM_`, though.

Because HTML is also a monoid, the above fragment would be equivalent to:

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

*QX*: How do you think about this syntax, generally?

*QX*: Do you think `!` is a good operator for setting attributes? We made an
initial choice for `!` because the old HTML package uses this. However, this 
operator looks more like array indexing. It is not too late to change this, 
suggestions are very welcome.

*QX*: How should multiple attributes be handled? In the above example, we used 
the `!` again for the next attribute:

    link ! rel "stylesheet" ! type "text/css" ! href "screen.css"

Another option would be to define a variant that takes a list of attributes:

    link !> [rel "stylesheet", type "text/css", href "screen.css"]

Or, we could use a typeclass to give the `!` different uses, and thus have:

    link ! [rel "stylesheet", type "text/css", href "screen.css"]

The last option will, however, introduce a more complicated type for attributes,
and a little more overhead.

Naming conventions
------------------

As we said before, our goal is to have the same name as the HTML tag for the
corresponding combinator. However, this is not always possible. A number of
elements/attributes cannot have a Haskell function with the same name. Two 
examples are "http-equiv" (Haskell doesn't like the '-' character) or "class" 
(which is a Haskell keyword). Therefore, we propose the following simple
ruleset:

1. Replace '-' with '_'.
2. Append '_' to Haskell keywords.

With these rules, we can make combinators for the complete HTML specification.

*QX*: Do you agree on these rules?

Rendering & encoding
--------------------

As said before, We will support input from both `String` and `Data.Text`
datatypes. These two datatypes support all Unicode codepoints, so the output
format should support all Unicode codepoints, too. If the output format does
not support all Unicode codepoints, an extra escaping phase needs to happen.

*QX*: Do we need support for "lossy" encodings, e.g. Latin-1? All desktop
browsers, and most mobile browsers support superior encodings like UTF-8.

Also, we would like to pose some questions about the output format. There seem
to be two major options:

- Output to an encoded lazy ByteString. The advantage of this scenario is that
  the result can be sent over the network directly and efficiently, for example
  with the network-bytestring[1] library.
- Output to a Text value. The advantage here is that we have a nicer separation
  of concerns (encoding would be separated from HTML generation).

The first option is definitely faster.

*QX*: Should we sacrifice some speed to have the second option as a possibility?

> main = do
>     putStrLn $ renderHtml page1
>     putStrLn $ renderHtml page2

[1]: http://hackage.haskell.org/package/network-bytestring
