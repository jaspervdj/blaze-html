Saturday, April 17th
====================

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
