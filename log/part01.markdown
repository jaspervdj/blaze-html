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
