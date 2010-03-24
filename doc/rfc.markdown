% BlazeHtml RFC
% Simon Meier on behalf of the BlazeHtml team
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

As long as these goals don't conflict with our main goal, we will try to satisfy
them as good as possible.

# Proposed Solution

Our solution called "BlazeHtml" is based on a typeclass `Html` which defines a
set of core combinators. The instances of the `Html` typeclass correspond to
concrete output formats. We call such an instances a "Renderer". Using the core
combinators provided by the `Html` typeclass, we build concrete combinators
for the various flavours of Html-like documents on top.

In the following sections, we present the `Html` typeclass, the efficient
rendering instances, and the concrete document combinators.


## Core Combinators

We fix the representation of text to the `Text` type provided by `Data.Text`.
An attributes is just a tuple of a text representing the key and a text
representing the value.

    type Attribute = (Text,Text)

We use a difference list of attributes to represent the accumulated attributes
to be applied to a html element.

    type AttributeManipulation = [Attribute] -> [Attribute]

We define an _html document_ to be a value of a type which is an instance of
the `Html` typeclass. We call the methods of the `Monoid` and the `Html`
typeclasses the _core combinators_. These are the ones which are actually
used in a compiled executable to represent a html document.

    class Monoid h => Html h where
        -- | A text leaf -- no escaping is done.
        unescapedText    :: Text -> h
        -- | A leaf element with the given tag name.
        leafElement      :: Text -> h
        -- | A node element with the given tag and inner document.
        nodeElement      :: Text -> h -> h
        -- | Modify the attributes of the outermost elements in the .
        modifyAttributes ::
            (AttributeManipulation -> AttributeManipulation) -> h -> h

Let us give an example based on the core combinators. Assuming that

    (<>) :: Monoid m => m -> m -> m
    (<>) = mappend

The HTML code

    <h1>BlazeHtml</h1>
    <img href="logo.png"/>
    <p> is a <em>blazingly</em>fast HTML generation library</p>

would be represented using the core combinators as

    (nodeElement "h1" $ unescapedText "BlazeHtml") <>
    (modifyAttributes (("href","logo.png"):) .) $ leafElement "img") <>
    (nodeElement "p" $ 
        unescapedText " is a " <>
        (nodeElement "em" $ unescapedText "blazingly") <>
        unescapedText " fast HTML generation library")

Obiviously, this is a quite wordy way to express Html documents. Therefore, we
build a whole set of actual combinators atop of these to conventiently write
html documents. We present the design of these _concrete combinators_ [better
name needed here] in the in section "Specyfing Html Documents".

Here, we are interested in the semantics of the core combinators. Apart from
the `Monoid` laws, each instance of the Html document satisfies at least the
following laws.
      
    unescapedText mempty = mempty
    unescapedText (t1 <> t2) = unescaptedText t1 <> unescapedText t2

    modifyAttributes f mempty = mempty
    modifyAttributes f (h1 <> h2) = modifyAttributes f h1 <> modifyAttributes f h2

    modifyAttributes f (unescapedText t) = unescapedText t
    modifyAttributes f2 (modifyAttributes f1 t) = modifyAttributes (f1.f2) t

Note that the equation

    nodeElement n mempty = leafElement n

is NOT guaranteed to hold, because `nodeElement n mempty` would produce
something of the form:

    <tag></tag>

Whereas `leafElement n` would rather produce something like:

    <tag />


## Rendering


## Encoding

Analyze and specify problem first!


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
naming clashes, between the elements and the `Prelude` (e.g. `id`), between the
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
