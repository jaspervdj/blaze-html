BlazeHtml
=========

[![Build Status](https://secure.travis-ci.org/jaspervdj/blaze-html.png?branch=master)](http://travis-ci.org/jaspervdj/blaze-html)

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

blaze-from-html
---------------

The `blaze-from-html` tool has moved to a separate repository:
<https://github.com/jaspervdj/blaze-from-html>.
