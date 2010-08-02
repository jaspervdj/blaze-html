---
title: Home

--- column1

BlazeHtml is a blazingly fast HTML combinator library for the [Haskell]
programming language. It embeds HTML templates in Haskell code for optimal
efficiency and composability.  To get started, just

    cabal install blaze-html

[Haskell]: http://haskell.org/

--- column2

The project is aimed at those who seek to write web applications in Haskell --
it integrates well with all Haskell web frameworks.

The best way to get started with BlazeHtml is to have a look at our [tutorial].

[tutorial]: $root/tutorial.html

--- body

# Features

- Pretty fast -- have a look at [our benchmarks]
- Lightweight DSL syntax
- Embedded in Haskell
- Efficient Unicode support
- Supports HTML 4 Strict and HTML 5
- Tool to create code from an HTML file

[our benchmarks]: $root/benchmarks.html

# Status

The BlazeHtml API is considered stable, however, the implementation is still
experimental. We like to encourage you to try it and tell us what you think, and
submit possible bugs to the [issue tracker].

[issue tracker]: http://github.com/jaspervdj/BlazeHtml/issues/

# Google Summer of Code

Jasper Van der Jeugt is working on this project for [Google Summer of Code]
2010. This means we have made a roadmap for this summer:

[Google Summer of Code]: http://code.google.com/soc/

- Initial release.
- Fix implementation such that it works for outputting UTF-8 encoded
  `ByteString`, `Text`, and `String`.
- Add further HTML, XHTML variants depending on requests.
- Document and, where possible, package implementation techniques such that
  other projects can use them; e.g. faster binary builder, `Utf8Builder`,
  `TextBuilder`, and `StaticMultiString`.
- If time remains: Write a BlazeHtml generator translating an HTML page to the
  corresponding BlazeHtml haskell code. This greatly simplifies initial imports
  and importing of new snippets.

# Code

If you are interested, all code is available on [GitHub].

[GitHub]: http://github.com/jaspervdj/BlazeHtml/
