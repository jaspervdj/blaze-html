BlazeHtml
=========

[![Hackage](https://img.shields.io/hackage/v/blaze-html.svg?color=informational)](https://hackage.haskell.org/package/blaze-html)
[![Stackage LTS](https://stackage.org/package/blaze-html/badge/lts)](https://stackage.org/lts/package/blaze-html)
[![Stackage Nightly](https://stackage.org/package/blaze-html/badge/nightly)](https://stackage.org/nightly/package/blaze-html)
[![Haskell-CI](https://github.com/jaspervdj/blaze-html/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/jaspervdj/blaze-html/actions/workflows/haskell-ci.yml)

What
----

BlazeHtml is a library that aims to be the fastest way to generate HTML in Haskell.
For more information, refer to [the website][].

[the website]: http://jaspervdj.be/blaze

How
---

We have a makefile to manage most tasks. First off, a part of the code is
generated automatically, so you should begin by running

    make combinators

Run the tests using

    make test

blaze-from-html
---------------

The `blaze-from-html` tool has moved to a separate repository:
<https://github.com/jaspervdj/blaze-from-html>.
