Saturday, April 17th, midday
============================

Hello, dear reader. My name is Jasper Van der Jeugt and these are my logs from
my work on the BlazeHtml project. I started out today cleaning the entire repo,
on a new branch "develop", because the old repo was getting a little to messy.

The first goal for the project is to define a baseline for efficiency. The idea
is to compare implementations on two different benchmarks:

- BigTable. This is the rendering of 1000x10 HTML table.
- Basic. This is a basic rendering test containing some different templating
  features.

These benchmarks were chosen because they are implemented for many templating
systems out there, and therefore, they are quite good for making comparisons.

Fast appending
--------------

One of the bottlenecks for efficiency is string appending. In our HTML
templating library, we have to concatenate lots of very small strings (e.g.
"<", "table", "/>") and slightly larger strings (dynamic and static text).
We will focus on UTF-8 output for now, so we can output `ByteString`s. One
way to concatenate characters to a `ByteString` is the `Data.Binary.Builder`
monoid. We will first examine (and probably patch) this `Builder`.

I'm patching against the binary-0.5.0.2 package on Hackage. I'll put these
other libraries under `lib/`, so our directory structured stays clean.

Benchmarks
----------

In `lib/binary-0.5.0.2`, I added a `BlazeBenchmarks.hs` file which will
benchmark the specific features of the `Data.Binary.Builder` that we need.
For now, we will have three benchmarks, testing the conversion of three
different types into a `Builder`:

- `[String] -> Builder`
- `[ByteString] -> Builder` (strict `ByteString`s)
- `Text -> Builder` (strict `ByteString`s)

The reason to choose for these benchmarks is that either `String` or
`ByteString` will probably be used for the little static pieces, and `Text` for
the dynamic, larger text fragments (or that's the plan for now, at least). On a
side note, we'll use criterion for our benchmarks, since that's the standard
these days.

This means we will have to encode the `Text` to UTF-8, while we can just
truncate the `String`s (the `ByteString`s are bytes already). With simple, naive
definitions for both benchmarks, we get:

- `[String] -> Builder`: 62.99059 us (std dev: 1.608421 us)
- `[ByteString] -> Builder`: 32.25011 us (std dev: 1.243526 us)
- `[Text] -> Builder`: 3.717675 ms (std dev: 93.80014 us)

Note that we compile with `-O2`, of course.

Overhead in fromByteString
--------------------------

First, a little work on the `[ByteString] -> Builder` benchmark. The problem
here is that we use the `fromByteString` function. These function assumes the
strict `ByteString`s we append to the builder are already "long enough", so the
function flushes the buffer. However, in our case, we're dealing with really
small strings -- so we don't want to flush the buffer, we want to write to the
buffer. So we patch the builder with a `fromByteString'` function:

    -- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
    --
    fromByteString' :: S.ByteString -> Builder
    fromByteString' byteString = writeN l f
      where
        (fptr, o, l) = S.toForeignPtr byteString
        f dst = do copyBytes dst (unsafeForeignPtrToPtr ptr `plusPtr` o) l
                   touchForeignPtr fptr
        {-# INLINE f #-}


This function should be more fitted for small strings. And indeed, we get
slightly better benchmarks:

- `[String] -> Builder`: 68.73607 us (std dev: 7.898973 us)
- `[ByteString] -> Builder`: 35.73695 us (std dev: 8.147190 us)
- `[ByteString] -> Builder'`: 26.59666 us (std dev: 3.947255 us)
- `[Text] -> Builder`: 3.978407 ms (std dev: 255.7193 us)

The second benchmark is definitely faster than the first one.

Saturday, April 17th, evening
=============================

This evening, I decided to tackle the problem of writing `Text` values in a
very fast way. Our function pipeline looks more or less like:

    convertToBuilder . encodeUtf8 . escapeHtmlCharacters

I don't see an elegant approach here. `escapeHtmlCharacters` cannot be fused
because `concatMap` cannot be fused. So, since we cannot directly work with the
string types, we would have a pipeline for every character.

    convertToBuilder . encode . escape

Then, it will be concatenated again using `mconcat` or something similar. The
downside of this approach is, however, that most characters will be dumb, stupid
characters -- and so our pipeline will cause an overhead. I don't know the exact
overhead though, a benchmark would be in place here.

But we will choose for efficiency, and raw speed. That's why I patched
`Data.Binary.Builder` with a huge and ugly function that embodies this entire
pipeline. The gain from our naive approach is huge:

- `[Text] -> Builder`: 3.664167 ms (std dev: 113.5493 us)
- `[Text] -> Builder'`: 744.9972 us (std dev: 27.23619 us)

And we must note that our naive approach did not include escaping, while the big
ugly function does.

Sunday, April 18th, morning
===========================

A little abstraction
--------------------

I decided to refactor some of the code I wrote yesterday. Remember the "big ugly
function"? I put it in `Data.Binar.Builder`, because it needed some `Builder`
internals. However, this function was UTF-8 _and_ HTML specific, so putting this
function there was no option because

- It _really_ doesn't belong there, concept-wise.
- The Binary Strike Team would never accept such a patch anyway.

First, I added some other functions we would need for producing Builders.

- `fromSmallByteString :: S.ByteString -> Builder` (this is actually the
  `fromByteString'` function from a little earlier, that I gave a proper name
  now).
- `fromUnicodeShow :: Show a => a -> Builder`
- `fromAscii7Show :: Show a => a -> Builder`
- `fromHtmlText :: Text -> Builder` (this is the big ugly function).

UTF-8 encoding was needed in both the `fromUnicodeShow` and `fromHtmlText`. And
very soon, patterns and abstractions started emerging. It turned out that
`writeN` was actually the only internal function that I needed, but that's not
really a good name to export, so I patched `Data.Binary.Builder` with

    -- | /O(n)./ A Builder from a raw write to a pointer.
    fromUnsafeWrite :: Int                  -- ^ Number of bytes to be written.
                    -> (Ptr Word8 -> IO ()) -- ^ Function that does the write.
                    -> Builder              -- ^ Resulting 'Builder'.
    fromUnsafeWrite = writeN
    {-# INLINE fromUnsafeWrite #-}

Results
-------

Now, I could write my functions in another file, so I chose
`Text.Blaze.Internal.Utf8Builder` for now. So I moved the benchmarks to another
directory as well and made it all work together again.

Initially, the abstraction introduced a small slowdown, unfortunately. But I was
able to get back to the speed before the abstraction by adding a little
inlining. If this approach works out in the future, we should probably try to
get this patch accepted by the Binary Strike Team.

Sunday, April 18th, around midday
=================================

Html benchmarks
---------------

We will start by running some benchmarks for _other_ templating libraries. I do
all work with an Dell Inspiron 6400 laptop. My CPU is a dual-core i686 32 bit
model, both cores are 800MHz, and I have 1GB of RAM. For some other templating
engines, I get the following results:

    Genshi tag builder                            931.63 ms
    Genshi template                               631.47 ms
    Genshi template + tag builder                1031.72 ms
    Mako Template                                  77.74 ms
    Kid template                                 1805.28 ms
    ClearSilver                                   116.32 ms
    Django template                               808.69 ms
    Spitfire template                              74.88 ms
    Spitfire template -O1                          39.45 ms
    Spitfire template -O2                          16.05 ms
    Spitfire template -O3                          16.10 ms
    Spitfire template -O4                          10.03 ms
    StringIO                                      109.05 ms
    cStringIO                                      22.27 ms
    list concat                                    13.79 ms

I wrote a library sketch in `benchmarks/Utf8Html.hs`, based on the previous
work of Simon Meier and me, but this time using the new `Builder` functionality.
We get the following initial results:

- `bigTable`: 11.02054 ms (std dev: 200.6569 us)
- `basic`: 32.39297 us (std dev: 684.6011 ns)

I think these results make a good initial baseline.

Sunday, April 18th, evening
===========================

I discovered a bug in the implementations. The problem was that
`fromUnicodeString` and `fromAscii7String` (two functions producing a `Builder`)
would actually `show` the `String` first. This led to two unnecessary quotes in
most situations.

In our original draft, we spoke of a "monadic" syntax. This is a very nice
feature to have, but we cannot have it if it makes our implementation less
efficient. That's why implemented another test in `benchmarks/Utf8Html.hs`,
using the monadic approach. The results are surprisingly well:

- `bigTable`: 10.84097 ms (std dev: 195.6227 us)
- `basic`: 32.09416 us (std dev: 368.2538 ns)
- `bigTableM`: 9.909449 ms (std dev: 126.3776 us)
- `basicM`: 28.18043 us (std dev: 349.1422 ns)

As you can see, they are even slightly faster than their non-monadic variants.
I have some theories about why this could happen, but some more research is
needed there.

Monday, April 19th, evening
===========================

Simon discovered some more inlining we could apply to our code, so we made some
improvements again. I also refactored `Text.Blaze.Internal.Builder` and
commented it where necessary. The benchmarks currently give me:

- `bigTable`: 7.698022 ms (std dev: 230.3451 us)
- `basic`: 23.82825 us (std dev: 1.130614 us)

Yay! We seem to be below "Spitfire template -O4" now!

Tuesday, April 20th, evening
============================

So, I splitted our current code out a little, into proper modules. I adapted the
benchmarks and there is no slowdown, yay. Next task is to write better
benchmarks. We need specialized benchmarks for at least:

1. leaf tags
2. nested tags
3. attributes
4. static string literals
5. dynamic strings
6. dynamic text

Also, I need to write all HTML tag and attribute functions. I will probably use
a simple automated solution here, since the code for all tags is nearly the
same.

Wednesday April 21th, evening
=============================

Bang
----

I re-introduced the `!` operator for attributes. I'm not sure if this is the
operator I want to go with because it kind of reminds of indexing. But the old
HTML library uses this one, so I might as well stick with it for now.

I also added some type safetype by introducing a `newtype Attribute`. It's
defined as a simple Html manipulator.

> newtype Attribute = Attribute (Html -> Html)

I changed the occurences of `addAttribute` in the code to the `!` operator, and
there was no slowdown, so I'm comitting it.

Setting attributes on non-leaf nodes
------------------------------------

We want a situation where you can use the `!` as well on leaf nodes as well as
on non-leaf nodes. I think the best way to do this is to creat a
`class Attributable`, and make two instances, `Html` and `Html -> Html`. I added
`{-# INLINE #-}` as well as `{-# SPECIALIZE #-}` pragmas for both instances.
And again, I noticed no slowdown.

Some benchmarks
---------------

I also added two very simple benchmarks: one creating very wide, shallow tree of
HTML, and once creating a very deep and small HTML tree. Of course, I can't
compare them with anything now, but I can use them to check for regressions in
the future.

Thursday, April 22th, evening
=============================

I'm currently a bit stuck on a piece of API design. I'll try to explain the
problem here as well, as clear as possible. Say, you have the regular div tag.
This would be, in our library, Text.Blaze.Html.Strict.div. That makes sense, but
it seems possible that a user wants to use a div as a leaf node (e.g.
`<div />`). I'm not sure what we want to do in that case. There seems to be a
number of options, as always:

- Provide leaf and non-leaf combinators for every tag. I'm not sure if this
  is overkill or not, but it is, for example not forbidden to have content in
  an `<img>` tag. We could have these functions in Text.Blaze.Html.Strict and
  Text.Blaze.Html.Strict.Leaf, for instance. But it must be kind of annoying
  for the end user to have to write `L.div` instead of just `div`. On the
  other hand, if we put the *common* uses for the tags (e.g. `img` as leaf,
  `div` as non-leaf) in the main module, we would get a very inconsistent
  mess, I assume.

- Do not provide closing tags for something like `div`, the user would just
  create `<div></div>` instead. I do not like this option at all for numerous
  obvious reasons.

- Provide some kind of operator to make a non-leaf node a leaf node and vice
  versa. Altough, I'm not sure if this would work well.

As you see, none of the options is really *very* convincing, altough the
first one seems the best for now. I'm not sure on this, so I asked some feedback
from both Simon and Johan.

Sunday, April 25th, morning
===========================

I have found another branch for the leaf/non-leaf problem. We could only support
parent nodes (of the type `Html -> Html`), and introduce another combinator:

    (/>) :: Html
    (/>) = mempty

I have chosen `/>` here because it resembles the end of a leaf HTML tag (e.g.
`<img />`). Then, we would introduce a custom rule.

    {-# RULES
        "tag/empty" forall x y. tag x y (/>) = leaf x
        #-}

The `y` here is the closing tag, we pass it as an argument for performance
reasons, and you can safely ignore it. This code results in the fact that if
we write

    img (/>)

somewhere in our template, it would be rendered to `<img></img>` when we don't
pass `-fenable-rewrite-rules` to the compiler, and `<img />` otherwise. Note
that `-O` implies `-fenable-rewrite-rules`. I'm not sure about this solution
either, because it sort of feels like a (slightly elegant) hack. That's why
I'm comitting the code for this to another branch called `rewrite-leafs`.

Sunday, Apr 25th, afternoon
===========================

I started some simple code in the `util/` directory to generate some code for
us. I'm talking about the actual HTML combinators -- we don't want to write that
code by hand. I implemented some very simple naming conventions:

1. When an HTML tag is a Haskell reserved keyword, we append an '_'.
2. When an HTML tag contains a '-' character, we replace it by an '_'.
3. No other transformations are applied.

I also made a list of elements that can't be leafs and elements that must be
leafs, based on the W3C reference.

Tuesday, April 27th, afternoon
==============================

I did some further work in the `util/` directory. My current idea is to have a
data structure called `HtmlVariant`, defined as:

    data HtmlVariant = HtmlVariant
        { attributes :: [String]
        , parents    :: [String]
        , leafs      :: [String]
        , version    :: String
        , variant    :: String
        } deriving (Show)

And then write a function `writeHtmlVariant` which will write the variant to
the correct modules. For strict HTML 5, for example, `version` would be
`"Html5"` and `variant` would be `"Strict"`. The code would then be written
to the modules:

    Text.Blaze.Html5.Strict
    Text.Blaze.Html5.Strict.Attributes

This seems like pretty good behaviour, since it would be easy to add different
Html variants later.

On a sidenote, this project got accepted for Google Summer of Code 2010! Woot
woot woot!

Wednesday April 28th, afternoon
===============================

Simon Meier alerted me that there are no variants in HTML 5. This means the code
I wrote yesterday afternoon is a little incorrect, especially the

    ...
    , version    :: String
    , variant    :: String
    ...

part. I now fixed it by using

    ...
    , version    :: [String]
    ...

instead. It's pretty simple, we just have a `[Major, Minor]` and optionally
`[Major]` version (actually, the lists can be as long as you want). This allows
us to have different variants of HTML 4 and a single version of HTML 5.

Saturday May 1st, midday
========================

We updated our planning a little yesterday. This is the updated planning:

- Finish metacode to generate tags, so we have the complete HTML 4 spec.
- Document and clean up the library, release as 0.0, together with a short RFC.
- Make improvements based on comments. It is hard to estimate how long
  this phase will will be.
- Benchmarks: A full set of benchmarks, so every part of the library is tested.
- Abstraction: I would still like to abstract the library over a
  typeclass, because this offers a lot more flexibilty. If we have
  benchmarks, we can see how much this abstraction costs us.
- Quickcheck and HUnit tests.
- Tutorials, packaging, etc.

Other than that, I finished the HTML generation code. It should be relatively
easy to add new HTML versions (the hardest part is really finding al tags and
attributes). I made sure documentation is generated as well.

The entire HTML 4.01 Strict specification is now available under
`Text.Blaze.Html4`.

Sunday, May 2nd, morning
========================

Today, I'm going to have a look at attributes. I wrote a quick benchmark that
sets 5000 attributes on an `<img />` element. The initial time is 13.04ms. I
could get it down to 10ms by using a similar approach as used with the leaf and
parent combinators.

I also tried to change the inlining location. For example, instead of having

    img = leaf "<img"

we probably want

    img = leaf "img"

Because that allows a more elegant interface. By constructing a `ByteString` in
the leaf function, I seemed to even get a slight speedup. This also got the
attributes benchmark down to 9ms.

Saturday, May 8th, evening
==========================

So, it's been a busy week because of a college project. I met (well, virtually
met) with Simon and Johan wednesday and we discussed a more detailed planning.

I started writing an RFC to send out to the haskell community, so I can get
feedback before the library design is too fixed.

Tuesday, May 11th, evening
==========================

Lately, I've been doing some more writing on the RFC. Also, I am playing around
with a typeclass idea again. It seems that I can get an implementation as fast
as the one we currently have using a typeclass like:

    class Monoid h => Html h where
        parent            :: S.ByteString -> h -> h
        leaf              :: S.ByteString -> h
        attribute         :: S.ByteString -> Text -> Attribute h
        string            :: String -> h
        text              :: Text -> h
        escapedByteString :: S.ByteString -> h

More methods need to be added though, but if this means we can be as fast using
a typeclass, this seems more attractive since it offers a lot of flexibility.

Saturday, May 15th, afternoon
=============================

After reading some more in the HTML 4.01 spec, I saw that it is recommended for
HTML 4.01 "leaf" elements (such as `img` and `br`) **not** to have a closing
tag. This means that for HTML 4.01, BlazeHtml should produce `<img>` instead of
`<img />`.

In order to realise this, I added another primitive. We now have `parent`,
`leaf` and `open` as low-level combinators. HTML variants can specify "open"
elements in addition to leafs and parents.

Sunday, May 16th, evening
=========================

This evening, I added document type definitions to BlazeHtml. The idea
is pretty simple: in a Html4.Strict document, you would just write:

    docType

Which would be rendered as:

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
            "http://www.w3.org/TR/html4/strict.dtd">

Wednesday, May 19th, afternoon
==============================

I made what is an important commit today (or at least, I think it's an
important commit). Remember the "big ugly but fast" function I used to get the
speed? I was able to split this function into two parts -- one doing UTF-8
logic, the other doing Builder logic. I think this is very good, because several
people were afraid that we would duplicate too much code that was already in the
Haskell libraries somewhere.

The key (to not getting any slowdown) is the type of the `encodeCharUtf8`
function. The function(s) already in the Haskell libraries usually have a type
signature like:

    encodeCharUtf8 :: Char -> [Word8]

This is no good for us, since the intermediate lists are too much of a
bottleneck. Therefore, we use a more abstract function:

    encodeCharUtf8 :: (Word8 -> a)
                   -> (Word8 -> Word8 -> a)
                   -> (Word8 -> Word8 -> Word8 -> a)
                   -> (Word8 -> Word8 -> Word8 -> Word8 -> a)
                   -> Char
                   -> a

You explicitly specify to this function what needs to happen for the different
lengths of an encoded Unicode character (1, 2, 3 or 4 bytes). Because we can
do some inlining, this approach is equally efficient (the benchmarks didn't
change).

Monday, May 24th, morning
=========================

Lately, I have been playing with an idea from Simon: a cached builder monoid.
I've been investigating this for a very specific purpose -- tags. Currently, the
`parent` function, for example, has the type signature:

    parent :: S.ByteString -> Html -> Html

We use a strict `ByteString` for the tag name. Advantages:

- Very fast.
- Very, very fast.

Disadvantages:

- It is not encoding independent. We rely on the fact the the lowest 128
  characters are the same for every encoding; and this is not the case. We could
  get away with this for now because we only support UTF-8, but this might a
  serious disadvantage for flexibility reasons in the future.

I thus implemented a `cached` function

    cached :: Utf8Builder -> Utf8Builder

that brings the data in a "more suited" format. Note that this `cached` function
only works well for very small builders. In the future, I will have two options:

- Extends this function so it works for all builders.
- Rename it to `prepareSmallBuilder` or something similar.

Internally, the `cached` function simply works by doing all encoding and
escaping, so the single copy can happen with a fast call to `copyBytes`.

Friday, May 28th, morning
=========================

A quick update. A renamed the `cached` function to `optimizePiece` for now, I
might change the name back again if I make the implementation more general.
Wednesday, Simon and I finished the RFC, and it was published yesterday. We're
getting lots of feedback, but I want to wait a little more to react on it.

What I'd like to do next is take our current `Utf8Builder` and split it into
two parts: an `Utf8Builder`, and an `Utf8HtmlBuilder`, where the former is
unaware of any HTML-specific issues.

The first thing to do is write a regression test, so I can see if performance
increases or decreases if I split it up. That's why I'm going to write a
`lotsOfEscaping` benchmark, which tests the HTML-specific performance of the
builder.

(Hacking...)

I abstracted away our builder a bit, and the result is that the escaping is
now optimized. The benchmark I wrote for escaping dropped from initially 1.61ms
to 1.12ms. Yay. There's also been a slight speedup in the other benchmarks as
well, because some optimizations were quite general.

Sunday, May 30th, morning
=========================

Correctness and type safety are very important for Haskell programmers. Until
now, we used `Html` for our main HTML type, where

    type Html = HtmlM ()

This means code like this was possible:

    page :: Html
    page = html $ do
        x <- p $ "Foobar"
        p $ string $ show x

The user would expect `show x` to give `()` back. But it doesn't -- it crashes
with an error. This is because `>>=` is not _really_ supported in our monad, for
efficiency reasons. However, we do not want the user to get errors like these
either.

I dropped the `Html` type alias, and moved `HtmlM` to `Html`. And instead of
using `Html ()` as main HTML type, we now use `Html a`.

    page :: Html a
    page = html $ do
        x <- p $ "Foobar"
        p $ string $ show x

It is now impossible to compile this snippet. This is because `a` is too
_general_ to actually do anything with it, and it can thus no longer be
evaluated (unless you're using type hacks, in which case you deserve the
error ;-)).

Thursday, June 3rd, morning
===========================

So, I notice I haven't written too much logs lately. I should certainly try to
increase my output here again, since it's interesting for myself as well as for
possible readers (more interesting than the git log, at least).

We discussed an implementation based on an algebraic datatype yesterday. This
means our `Html` type would be an actual datatype:

    data Html = ...

instead of the closures and newtypes we now use. This approach gives us
excellent flexibility, but the question is if we can get it fast enough. I just
added an initial test implementation, it appears to be twice as slow as the
current performance of the develop branch. But, of course, there's room for
optimization.

Friday June 4th, afternoon
==========================

We decided it was time for a more advanced and real-world benchmark, so I wrote
a bigtable HTTP server. The idea is very simple: you run the server like

    make bench-bigtableserver
    ./benchmarks/BigTableServer 3000

You can then navigate to `localhost:3000/100/50` using your favorite webbrowser
and you will an HTML table of 100 rows and 50 columns. You can then measure the
performance using a tool like `httperf` or `ab`.
