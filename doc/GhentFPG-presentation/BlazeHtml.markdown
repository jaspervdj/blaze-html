% BlazeHtml
% Jasper Van der Jeugt
% April 1, 2010

# Hello

I'm Jasper. I started the `BlazeHtml` project.

> @jaspervdj  
> jaspervdj.be  

# The problem

A web server usually consists of three layers:

- A web application server.
- A data storage layer.
- An HTML generation library.

# An HTML generation library

In `BlazeHtml` we want:

- Efficiency.
- Composability.
- Extensibility.
- Clear syntax.

# Interlude: HTML is a Monoid

A `Monoid` is a standard Haskell typeclass which supports two operations:

- `mempty`: an empty `Monoid` -- this is here, of course, an empty string.
- `mappend`: appending of two `Monoid`'s. This is nesting on the same level.

Example:

~~~~~{.haskell}
html1 `mappend` html2 `mappend` mempty
~~~~~

# Efficiency

Haskell is a compiled language. This fact should be abused.

- Aggressive inlining and optimizations.
- Use of `Data.Text` instead of `String`.

The problem is really the concatenation of lots and lots of really small
text fragments.

- We use the `Monoid` from `Data.Binary.Builder`.

# Composability

An `Html` snippet is a _first class value_, so it can easily be

- stored in variables
- concatenated from a list
- passed as parameter to a function
- returned from a function

# Extensibility

`Html` is a typeclass. Instances can be called "Renderers". This allows the
use to specify a custom "Renderer", or use a standard:

- `Data.Text` renderer
- `Data.ByteString` renderer
- Pretty printing renderer
- `Socket` renderer

# Clear syntax

We cheat and create a `Monad` over our `Monoid` with an unused type variable.

~~~~~{.haskell}
newtype HtmlMonad h a = HtmlMonad
    { runHtmlMonad :: h
    }

instance (Monoid h) => Monoid (HtmlMonad h a)
instance (Html h) => Html (HtmlMonad h a)
~~~~~

# More syntactic sugar

Using the `{-# LANGUAGE OverloadedStrings #-}` pragma, you can define cusom
string literals.

~~~~~{.haskell}
instance (Html h) => IsString (HtmlMonad h a)
~~~~~

# An example

~~~~~{.haskell}
myHtml :: (Html h) => h
myHtml = runHtmlMonad $ do
    let section t = h1 ! (class_ "dumb") $ t

    section "BlazeHtml -- Introduction"
    img ! name "logo.png" 

    H.div ! [A.id "fancy", A.id "foo"] $ do
        "A blazingly fast HTML combinator library."
        "Based on a Builder Monoid."
        em $ "With a pretty monad interface."

    section "BlazeHtml -- Problem"
    "..."
~~~~~

# That's it

## Questions?
