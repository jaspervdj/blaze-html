BlazeHtml
=========

What
----

BlazeHtml is a library that aims to be the fastest way to generate HTML in
Haskell. It's currently in a very experimental state, as different ideas and
designs are tried.

I keep a log of my actions in the `log/` directory, you can find more
information there.

Running
-------

To run the Utf8Builder benchmark:

    ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src benchmarks/Utf8Builder.hs
    ./benchmarks/Utf8Builder

To run the Utf8Html benchmark:

    ghc --make -O2 -fforce-recomp -isrc -ilib/binary-0.5.0.2/src benchmarks/Utf8Html.hs
    ./benchmarks/Utf8Html
