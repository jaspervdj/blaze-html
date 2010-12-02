BlazeHtml
=========

What
----

BlazeHtml is a library that aims to be the fastest way to generate HTML in
Haskell. It's currently in a very experimental state, as different ideas and
designs are tried. For more information, refer to [the website][].

[the website]: http://jaspervdj.be/blaze

How
---

We have a makefile to manage most tasks. First off, a part of the code is
generated automatically, so you should begin by running

    make combinators

Run the tests using

    make test

And the benchmarks using

    make benchmark

Cabal
-----

This directory contains two `.cabal` targets: `blaze-html.cabal` and
`blaze-from-html.cabal`. Cabal, however, only accepts one `.cabal` file in a
directory. We have a simple `Makefile` system to switch between the targets:

    make blaze-html
    make blaze-from-html

to select the desired target.
