---
title: Tutorial

--- column1

We hope to offer a small but comprehensible tutorial here. At the end of this
tutorial, you should be able to write all sorts of HTML templates using
BlazeHtml.

--- column2

Please note that you should *at least* know some basic Haskell before starting
this tutorial. [Real World Haskell] and [Learn You a Haskell] are two good
starting points.

[Real World Haskell]: http://book.realworldhaskell.org/
[Learn You a Haskell]: http://learnyouahaskell.com/

--- body

Installation
============

The installation of BlazeHtml should be painless using `cabal install`:

    [jasper@alice ~]$ cabal install blaze-html

Overloaded strings
==================

The `OverloadedStrings` is not necessarily needed to work with BlazeHtml, but it
is highly recommended, as it allows you to insert string literals in your HTML
templates without having boilerplate function calls.

> {-# LANGUAGE OverloadedStrings #-}

Modules
=======

This tutorial is a literate haskell file, thus we should begin by importing the
modules we are going to use. To avoid name clashes, we just import everything
twice -- once with an alias and once without.

> import Prelude
> import qualified Prelude as P
> import Control.Monad (forM_)

> import Text.Blaze.Html5
> import Text.Blaze.Html5.Attributes
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A

As you can see, we imported the `Html5` modules. Alternatively, you can choose
to import `Text.Blaze.Html4.Strict`. More HTML versions are likely to be added
in the future.

A first simple page
===================

The main representation type in BlazeHtml is `Html`. Therefore, your "templates"
will usually have the type signature `ArgumentType1 → ArgumentType2 → Html`.
We will now write a small page that just contains a list of natural numbers up
to a given `n`.

> numbers :: Int -> Html

Note how these templates are pure. It is therefore not recommended to mix them
with `IO` code, or complicated control paths, generally -- you should separate
your "view" code from your "logic" code -- but you already knew that, right?

> numbers n = html $ do
>     H.head $ do
>         H.title "Natural numbers"
>     body $ do
>         p "A list of natural numbers:"
>         ul $ forM_ [1 .. n] (li . showHtml)

Attributes
==========

We also provide combinators to set attributes on elements. Attribute setting
is done using the `!` operator.

> simpleImage :: Html
> simpleImage = img ! src "foo.png"

Oh, wait! Shouldn't images have an alternate text attribute as well, according
to the recommendations?

> image :: Html
> image = img ! src "foo.png" ! alt "A foo image."

As you can see, you can chain multiple arguments using the `!` operator. Setting
an attribute on an element with context also uses the `!` operator:

> parentAttributes :: Html
> parentAttributes = p ! class_ "styled" $ em "Context here."

As expected, the attribute will only be added to the `<p>` tag, and not to the
`<em>` tag. This is an alternative definition, equivalent to `parentAttributes`,
but arguably less readable:

> altParentAttributes :: Html
> altParentAttributes = (p $ em "Context here.") ! class_ "styled"

Nesting & composing
===================

It is very common to nest, compose and combine multiple templates, snippets or
partials -- use whatever terminology you prefer here. Again, a small example.
Say we have a simple datastructure:

> data User = User
>     { getUserName :: String
>     , getPoints   :: Int
>     }

If the user is logged in, we want to have a snippet that displays the user's
current status.

> userInfo :: Maybe User -> Html
> userInfo u = H.div ! A.id "user-info" $ case u of
>     Nothing ->
>         a ! href "/login" $ "Please login."
>     Just user -> do
>         "Logged in as "
>         string $ getUserName user
>         ". Your points: "
>         showHtml $ getPoints user

Once we have this, we can easily embed it somewhere else.

> somePage :: Maybe User -> Html
> somePage u = html $ do
>     H.head $ do
>         H.title "Some page."
>     body $ do
>         userInfo u
>         "The rest of the page."

In the previous example, the user would probably be pulled out of a reader
monad instead of given as an argument in a realistic application.

Getting the goods
=================

Now that we have constructed a value of of the type `Html`, we need to _do
something_ with it, right? You can extract your data using the `renderHtml`
function which has the type signature `Html → L.ByteString`. This function can
be found in the `Text.Blaze.Renderer.Utf8` module.

A lazy `ByteString` is basically a list of _byte chunks_. The list of byte
chunks the `renderHtml` is your HTML page, encoded in UTF-8. Furthermore, all
chunks will be nicely-sized, so the overhead is minimal.

There are other renderers as well -- for example there is a prettifying renderer
called `Text.Blaze.Renderer.Pretty`, and if you just want a `String`, use
`Text.Blaze.Renderer.String`.

The blaze-from-html tool
========================

There is also a tool called `blaze-from-html` which is used to convert HTML
pages to Haskell code using the BlazeHtml library. Usage is pretty
straightforward. An example:

    [jasper@alice ~]$ curl http://jaspervdj.be/blaze | blaze-from-html

will output the Haskell code that would be needed to produce
(this page)[http://jaspervdj.be/blaze]. By default, `blaze-from-html` will use
HTML5. You can use other variants as well:

    [jasper@alice ~]$ blaze-from-html -v html4-transitional index.html

To include the imports as well, use the `-i` flag. The `-e` flag causes
`blaze-from-html` to ignore a lot of errors, which might come in handy if the
page you are trying to convert has some faults in it.

Further examples
================

This tutorial should have given you a good idea of how BlazeHtml works. We have
also provided some more real-world examples, you can find them all in [this
directory] on github.

[this directory]: http://github.com/jaspervdj/BlazeHtml/tree/master/doc/examples/

Go forth, and generate some HTML!
