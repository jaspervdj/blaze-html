% BlazeHtml RFC
% Authors: Jasper van der Jeught and Simon Meier
% March 21, 2010

Introduction
============

This RFC describes the problem of efficiently generating Html responses for
sending them over the network and our proposed solution called "BlazeHtml".
The goal of this RFC is to gather feedback from the community about its design
before publishing the first implementation.

The outline of this document is as follows. First, we define the problem we
want to solve. Then, we describe the design originated from the ideas we
developed and prototyped at ZuriHac'10. Finally, we discuss further open
questions not related directly to one of the library's components.

Problem
=======

The problem we want to solve is how to efficiently generate Html responses for
sending them over the network.

The above goal is the main motivation behind our work. However, we also pursue
the following additional goals:

- static ensurance of syntactic well-formedness
- light-weight syntax for specifying Html documents
- support for other Html-like document formats (e.g.  XHtml, XML)
- html documents should be first-class values; i.e. we want composability
- it should be possible to write a document in an encoding-independent way and
  fix the encoding later

As long as these goals don't conflict with our main goal, we will try to satisfy
them as well as possible.

Proposed Solution
=================

Our solution called "BlazeHtml" is based on a typeclass `Html` which defines a
set of core combinators. The instances of the `Html` typeclass correspond to
concrete output formats. We call such an instances a "Renderer". Using the core
combinators provided by the `Html` typeclass, we build concrete combinators
for the various flavours of Html-like documents on top.

In the following sections, we present the `Html` typeclass, the efficient
rendering instances, and the concrete document combinators.

> {-# LANGUAGE OverloadedStrings #-}
> import Prelude hiding (head)
> import Data.Monoid (mempty)
> import Html.Strict
> import Html.Strict as H
> import Html.Strict.Attributes
> import Html.Strict.Attributes as A
> import Internal.Html
> import Internal.HtmlMonad
> import Internal.Attributes
> import Internal.Escaping

> import Renderer.Utf8HtmlRenderer
> import Renderer.Latin1HtmlRenderer
> import Renderer.Ascii7HtmlRenderer

> import Criterion.Main
> import qualified Data.ByteString.Lazy as BL
> import Control.Monad (forM_)
> import Data.Text (Text)
> import qualified Data.Text as T

Defining a document
===================

One of the goals of BlazeHtml is a light-weight syntax. Haskell supports this
for Monads using the do-notation, so we would like to use this syntax here as
well. We use `>>` for appending, and this means we can drop an operator for
concatenating two HTML snippets (which is probably the most common operation
you use when creating documents).

> testSnippet :: Html h => h
> testSnippet = separatedHtml $ do
>     let section t = h1 ! (class_ "dumb") $ text t
> 
>     section "BlazeHtml -- Introduction"
>     img ! href "logo here.png" ! alt "Alt text."
> 
>     H.div ! A.id "fancy" $ separatedHtml $ do
>         "A blazingly <fast> HTML combinator library."
>         "Based on a Builder Monoid."
>         em $ "BlazeHtml also supports a monad interface."
> 
>     section "BlazeHtml -- Problem"
>     "Hello world. λf.(λx.fxx)(λx.fxx)"
>     "These & those."

> testDoc :: Html h => h
> testDoc = concatenatedHtml $ do
>     html $ do head mempty
>               body testSnippet

If you load this file in `ghci`, you can now try:

- `renderUtf8Html testDoc`
- `renderLatin1Html testDoc`
- `renderAscii7Html testDoc`

You will see that, for every renderer used, the output will be in the correct
format and will contain the correct encoding tag.

__Open Question__: should the head `meta` tag be automatically set, or should we
solve that with a separate combinator `headWithMetaEncoding`?

Some Benchmarks
===============

> bigTable :: Int -> BL.ByteString
> bigTable n = renderUtf8Html $ doc n
>   where
>     doc :: Html h => Int -> h
>     doc n = table $ concatenatedHtml $
>         forM_ [1..n] $ \row ->
>             tr $ forM_ (zip ['a'..'j'] [1..10]) $ \col ->
>                 td $ text $ T.pack (show col)

> basic :: (Text, Text, [Text]) -- ^ (Title, User, Items)
>       -> BL.ByteString
> basic (title', user, items) = renderUtf8Html $ concatenatedHtml $ html $ do
>     head $ do title (text title')
>     body $ do H.div ! A.id "header" $ do
>                   h1 (text title')
>               p ("Hello, " >> text user >> "!")
>               p "Hello, me!"
>               p "Hello, world!"
>               h2 "Loop"
>               ul $ mapM_ (li . text) items
>               H.div ! A.id "footer" $ mempty

> main = defaultMain
>     [ bench "render/length bigTable" $ nf (BL.length . bigTable) rows
>     , bench "render/length basic" $ nf (BL.length . basic) basicData
>     ]
>   where
>     rows :: Int
>     rows = 1000
>
>     basicData :: (Text, Text, [Text])
>     basicData = ("Just a test", "joe", items)
>     items :: [Text]
>     items = map (T.pack . ("Number " ++) . show) [1 .. 14]

Acknowledgments
===============

Thanks to Johan Tibell for coming up with this nice summer of code proposal.
Jasper van der Jeugt for putting it on the ZuriHac project list.

The BlazeHtml team 

  Chris Done
  Fred Ross
  Jim Whitehead
  Japser van der Jeugt
  Harald Holtmann
  Oliver Mueller
  Simon Meier
  Tom Harper

at ZuriHac for making the first prototype of BlazeHtml real.
