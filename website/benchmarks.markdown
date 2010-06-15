---
title: Benchmarks

--- column1

Speed is an important design goal for BlazeHtml. Hence, it is pretty fast. Here,
we present some very simple benchmark results. All benchmarks have been executed
on the same machine, an Intel CPU T2080 @ 1.73GHz.

--- column2

We implemented the benchmarks for the rendering engines that do not provide
the code for the benchmarks themselves, the resulting code can be found [in our
repo]. The python benchmarks were implemented by the [spitfire project].

[in our repo]: http://github.com/jaspervdj/BlazeHtml/tree/master/benchmarks/bigtable/
[spitfire project]: http://code.google.com/p/spitfire/

---

# Bigtable

![bigtable benchmark results]($root/images/benchmarks-bigtable.png)

The bigtable benchmark consists of the rendering of a big HTML `<table>`. More
precisely: a 1000x10 HTML table, with `[1 .. 10]` as row content.
