Html templating benchmarks
==========================

Introduction
------------

The goal of this document is to create a fixed set of benchmarks, that can be
implemented in all HTML templating libraries. The goal is a fair comparison
between the different libraries (in different languages).

Big table
=========

The big table benchmark is simply the creation of a big table.

Input
-----

A two-dimensional array. The concrete input case is

Output
------

An HTML table containing the contents of this two-dimensional array. You can
assume the array data does not need escaping.

Test data
---------

An array with 1000 rows and 10 columns. Every row simply contains the values

    1, 2, 3, 4, 5, 6, 7, 8, 9, 10

Basic
=====

The basic benchmark is a very simple page.

Input
-----

A title, a username, and a list (or array, whatever fits the language used best)
of items. This are all strings.

Output
------

This is the expected output:

    <html>
        <head>
            <title>$title</title>
        </head>
        <body>
            <div id="header">
                <h1>$title</h1>
            </div>
            <p>Hello, $username!</p>
            <p>Hello, me!</p>
            <p>Hello, world!</p>
            <h2>loop</h2>
            <ol>
                <li>$item1</li>
                <li>$item2</li>
                <li>$item3</li>
                ...
            </ol>
            <div id="footer" />
        </body>
    </html>

Of course, indenting can differ, and a doctype can be added if this is the
default. Furthermore, `<div id="footer" />` may also be written as `<div
id="footer"></div>`.

Test data
---------

"Just a test" as title, "joe" as username, and "Number 1", "Number 2", "Number
3", ..., "Number 14" as items.

Wide tree
=========

Input
-----

A list (or again, an array) of strings.

Output
------

This is the expected output:

    <div>
        <p id="foo">string1</p>
        <p id="foo">string2</p>
        <p id="foo">string3</p>
        ...
    </div>

Test data
---------

The list "λf.(λx.fxx)(λx.fxx)", "These old days", "Foobar", "lol", "x ∈ A"
repeated a 1000 times.

Wide tree escaping
==================

This benchmark is the the wide tree benchmark again, with different test data.
In this benchmark, the templating library is *required* to treat the input data
as unsafe, and thus escape the HTML entities.

Test data
---------

The list "<><>", "\"lol\"", "<&>", "'>>'" repeated 250 times.

Deep tree
=========

Input
-----

A simple integer that gives a hint on how deep the tree should be.

Output
------

A deeply nested tree. With input 1, the output would be:

    <p>
        <table>
            <tr>
                <td>
                    <div>
                        foo
                    </div>
                </td>
            </tr>
        </table>
    </p>

With input 2, the above tree is again nested in p, table, tr, td, div elements.
So, with this recursive definition, we produce a tree with depth `input * 5`.

Test data
---------

1000

Many attributes
===============

Input
-----

A list of strings.

Output
------

An img element, with the id attribute set multiple times. For example:

    <img id="$string1" id="$string2" id="$string3" ... />

Test data
---------

The same list/array as used in the wide tree benchmark.
