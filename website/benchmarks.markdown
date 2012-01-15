---
title: Benchmarks
---

BlazeHtml is designed to be fast. In the context of an HTML generation library,
"fast" refers to the time that it takes a webserver for generating and sending
off the response.

As all benchmarks, these benchmarks are a little flawed. We cannot fairly time
the time it takes to send off the response, because different templating systems
are tied to different webservers. Therefore, we measure the time it takes us to
assemble the result of a template in memory.

All benchmarks have been executed on the same machine, a 2 Core Intel CPU T2080
@ 1.73GHz.

We also have to be very careful about *what* we are actually producing and
measuring. BlazeHtml produces a lazy `ByteString`. We have seen that the
chunksize of this `ByteString` can be important: [this is a graph] of a
benchmark we did where a large (128 kilobyte) HTML page is sent, with varying
chunk sizes.

[this is a graph]: /images/benchmarks-chunksize.png

This chunksize is now fixed for BlazeHtml: every chunk will contain around 32k
bytes.

<div id="fold" />

# Bigtable

The bigtable benchmark is a simple (artificial) benchmark implemented by many
HTML templating engines. It consists of the task of generating a 1000x10 HTML
`<table>`, with `[1 .. 10]` as row content. Some implementations of this
benchmark check if the row content needs to be escaped and some do not. In
BlazeHtml, we escape all content by default, as we want to free the user from
thinking about escaping. Obviously, the user can always explicitly state that he
doesn't want some content to be escaped.

![bigtable benchmark results](/images/benchmarks-bigtable.png)

# Running the benchmarks yourself

Get the BlazeHtml repo.

    git clone git://github.com/jaspervdj/blaze-html.git
    cd blaze-html

Run the ruby/php/... benchmarks. This requires you to have the different
templating systems installed, of course.

    make bench-bigtable-non-haskell

Run the BlazeHtml HTML benchmarks. The benchmark discussed above is called
`bigTable`.

    make bench-html

The python benchmarks are located in the [spitfire] repository. Check out the
code first:

[spitfire]: http://code.google.com/p/spitfire/

    svn checkout http://spitfire.googlecode.com/svn/trunk/ spitfire-read-only
    cd spitfire-read-only

And then run the benchmarks.

    python tests/perf/bigtable.py
