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
benchmark the specifix features of the `Data.Binary.Builder` that we need.
For now, we will have threee benchmarks, testing the conversion of three
different types into a `Builder`:

- `[String] -> Builder`
- `[ByteString] -> Builder` (strict `ByteString`s)
- `Text -> Builder` (strict `ByteString`s)

The reason to choose for these benchmarks is that either `String` or
`ByteString` will probably be used for the little static pieces, and `Text` for
the dynamic, larger text fragments (or that's the plan for now, at least). On a
sidenote, we'll use criterion for our benchmarks, since that's the standard
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

    converToBuilder . encodeUtf8 . escapeHtmlCharacters

I don't see an elegant approach here. `escapeHtmlCharacters` cannot be fused
because `concatMap` cannot be fused. So, since we cannot directly work with the
string types, we would have a pipeline for every character.

    converToBuilder . encode . escape

Then, it will be concatenated again using `mconcat` or something similar. The
downside of this approach is, however, that most characters will be dumb, stupid
characters -- and so our pipeline will cause an overhead. I don't know the exact
overhead though, a benchmark would be in place here.

But we will choose for effiency, and raw speed. That's why I patched
`Data.Binary.Builder` with a huge and ugly function that embodies this entire
pipeline. The gain from our naive approach is huge:

- `[Text] -> Builder`: 3.664167 ms (std dev: 113.5493 us)
- `[Text] -> Builder'`: 744.9972 us (std dev: 27.23619 us)

And we must note that our naive approach did not include escaping, while the big
ugly function does.
