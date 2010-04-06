% BlazeHtml RFC
% Authors: Jasper van der Jeught and Simon Meier
% March 21, 2010

# Introduction

This RFC describes the problem of efficiently generating Html responses for
sending them over the network and our proposed solution called "BlazeHtml".
The goal of this RFC is to gather feedback from the community about its design
before publishing the first implementation.

The outline of this document is as follows. First, we define the problem we
want to solve. Then, we describe the design that we developed and prototyped at
the ZuriHac'10. Finally, we discuss further open questions not related directly
to one of the library's components.

# Problem

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

# Proposed Solution

Our solution called "BlazeHtml" is based on a typeclass `Html` which defines a
set of core combinators. The instances of the `Html` typeclass correspond to
concrete output formats. We call such an instances a "Renderer". Using the core
combinators provided by the `Html` typeclass, we build concrete combinators
for the various flavours of Html-like documents on top.

In the following sections, we present the `Html` typeclass, the efficient
rendering instances, and the concrete document combinators.

## Core Combinators

Our `Html` typeclass depends on two other typeclasses, `UnicodeSequence` and
`Attributable`. In the next paragraphs, we will examine these three
typeclasses.

### UnicodeSequence

`UnicodeSequence` is a type that represent sequences of text. They can be built
from different types, including 7-bit ASCII characters, standard Haskell
characters, and `Data.Text` strings.

    class Monoid s => UnicodeSequence s where
        unicodeASCII7  :: Word8 -> s
        unicodeChar    :: Char  -> s
        unicodeText    :: Text  -> s

Implementations of this `UnicodeSequence` typeclass will use fast builder
Monoids, like `Data.Binary.Builder`, or the upcoming `Data.Text.Builder` Monoid.
In the code, we provide an instance for a fast `UnicodeSequence` using the
`Data.Binary.Builder` monoid. The benefit of this approach is that we can have
fast, zero-copy construction of our final (encoded) Document representation.

### Attributable

We have a separate typeclass to abstract values that can take attributes.

    class Attributable h where
        addAttribute :: h -- ^ Key.
                     -> h -- ^ Value.
                     -> h -- ^ Html taking attributes.
                     -> h -- ^ Result.

### Html

Finally, we have a class for abstracting HTMl documents trough closures.

    class (Attributable h, UnicodeSequence h) => Html h where
        separate    :: h -- ^ Left HTML.
                    -> h -- ^ Right HTML.
                    -> h -- ^ Result.
        leafElement :: h -- ^ Tag.
                    -> h -- ^ Result.
        nodeElement :: h -- ^ Tag.
                    -> h -- ^ Inner HTML.
                    -> h -- ^ Result.

The `separate` function is a way to guarantee some spacing between two HTML
elements. Appending without any space can be done using `mappend`.

Note that the equation

    nodeElement tag mempty = leafElement tag

is NOT guaranteed to hold, because `nodeElement tag mempty` would produce
something of the form:

    <tag></tag>

Whereas `leafElement tag` would rather produce something like:

    <tag />

## Rendering

As we said before, a "Renderer" is basically an instance of the `Html`
typeclass, which is able to flatten HTML documents to a sequence of characters.
We will provide several renderers, and it will be possible to implement your
own, custom renderer. We want to provide, at least:

- A very fast renderer which produces a lazy `ByteString`, by using the
  `Data.Binary.Builder` Monoid.
- A very fast renderer which produces lazy `Data.Text`, by using the
  `Data.Text.Builder` Monoid.
- A pretty printing library for debugging purposes.

All of these renderers can be trivially extended to render directly to a socket
or a file.

## Encoding

HTML documents can be sent in many encodings these days. We want to ease this
task, too, in BlazeHtml. The goal is that one can specify an entire HTML
document in an encoding-independent way, and fix the encoding later. For this
purpose, we have an `Html` instance that gives us the possibility of using
an arbirtary encoding tag, for example:

    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

This typeclass is thus specified like:

    newtype EncodingExplicit h = EncodingExplicit
        { runEncodingExplicit :: h -> h }

And the placeholder for the encoding tag is implemented like this:

    encodingTag :: EncodingExplicit h
    encodingTag = EncodingExplicit id

## Specifying Html documents

We think the syntax for HTML pages should be as light-weight as possible, this
will greatly improve readability. This is why we propose to define a Monad
wrapping the `Html` typeclass. The `>>` operator will be implemented as
`mappend`, so it is possible to write, for example:

    myHtml :: (Html h) => h
    myHtml = runHtmlMonad $ do
        p $ text "This is a first paragraph."
        p $ text "This is a second."

The `text` function here is defined as `unescapedText . escapeHtml`.

For setting attributes, we propose to use the `!` operator, like the old `HTML`
package does. We overloaded this operator so you can use it in different
situations:

[Open question: Should we move to another operator with less use in other
libraries? Moreover, `!` somehow always reminds me of indexing, which does not
agree with its semantics in this context.]

- Setting a single attribute.
- Setting a list of attributes.
- Setting a single attribute for a nested HTML element.
- Setting a list of attribute for a nested HTML element.

Examples for all these cases:

    myHtml :: (Html h) => h
    myHtml = runHtmlMonad $ do
        img ! src "logo.png"
        img ! [src "logo.png", alt "Logo of the company"]
        p ! name "fancy" $ do
            text "Some paragraph."
        p ! [name "fancy", id "second"] $ do
            text "Some paragraph."

Because the HTML elements all have short and general names, there are a lot of
name clashes between the elements and the `Prelude` (e.g. `id`,`div`), between the
elements and the attributes (e.g. `style`), and between the elements and
standard Haskell keywords (e.g. `class`).

This is why we need a strict and clear naming policy. We propose the following:

Elements and attributes are stored in two separated modules. If you import
them with an alias, you can use them safely. (e.g. `E.style` and `A.style`). If
possible, the HTML elements and BlazeHtml combinators should have the same name.
There are, unfortunately, a few exceptions we need to make:

- We append `_` to Haskell keywords (e.g. `class_`).
- We change `-` into `_` (e.g. `http_equiv`).

Finally, we add a last bit of syntactic sugar. In the `HtmlMonad`, we make an
instance of `GHC.Exts.IsString`. This results in the fact that you can drop the
`text` function when using the monadic notation. An example:

    myHtml :: (Html h) => h
    myHtml = runHtmlMonad $ do
        h1 $ "This is a header"
        p $ "This is a first paragraph."
        "This text is not inside any tag for now."

Of course, `unescapedText` will also be exported, as a back door for inserting
raw chunks of HTML, since skipping the escape function is obviously faster.


# Our Implementation

Explain how the implementation works like.

## Measurements

# Conclusion

Make clear what are the open questions.


# Acknowledgments

[In Chronological Order]

Thanks to Johan Tibell for coming up with this nice summer of code proposal.
Jasper van der Jeugt for putting it on the ZuriHac project list

The BlazeHtml team 

  Chris Done
  Jim Whitehead
  Japser van der Jeugt
  Harald ...
  Oliver Mueller
  Simon Meier
  Tom Harper

at ZuriHac for making the first version of BlazeHtml real.
