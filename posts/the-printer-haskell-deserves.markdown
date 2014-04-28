---
date: 2014-04-26
title: The printer Haskell deserves
description: The printer Haskell deserves
author: Chris Done
tags: haskell
---

Friday night project ahoy!

## Problem

I was working with haskell-names the other day. Its data types are
nice enough, but are rather unweildly to read in the REPL when
debugging and inspecting. This got me thinking about inspection and
printers for Haskell data structures again.

I've made several approaches for to haskell-mode in the past.

* One which requires parsing the output of Show with Haskell and then
  printing that to s-expressions for Emacs to consume. This is generally
  unreliable and hacky.
* Then I settled with making the REPL just syntax highlight the
  output. That generally works flawlessly and is an okay solution.
* Then I really wanted collapsing support again, so I implemented one
  based on Emacs's awareness of expression boundaries (of ( ) and { }
  and " " etc.). Simple. Kind of reliable.

Today I implement yet another one, but this one I like best. I've
always wanted to have a Haskell printer that can evaluate on demand,
piece-wise, taking care not to evaluate the whole structure too
eagerly. I should be able to type `[1..]` into my REPL and not be
spammed by numbers, but rather to expand it at my leisure.

## Implementation

My plan was to use the
[Data.Data API](http://chrisdone/posts/data-typeable) to traverse data
structures breadth-first, display to the user something like `Just …`
and then allow the user to continue evaluating on request by clicking
the `…` slot.

I chatted with [Michael Sloan](https://github.com/mgsloan) about it
and we came up with a simple experimental design and thought it would
be a nice idea. We hypothesized a nice class-based way to provide
custom presenters for your types, so that e.g. a `Diagram` could be
rendered as a bitmap inline with the rest of the data structure, but
that needs more thinking about.

I've implemented a basic version of it in the
[present](http://hackage.haskell.org/package/present-0.0.0) package (a
la “presentations” in
[CLIM](http://bauhh.dyndns.org:8000/clim-spec/23-1.html#_1125)) and
implemented a usable front-end for it in Emacs. There's some
information about the implementation
[in the README](https://github.com/chrisdone/present) which you can
read on Github.

## Result

Yes! It
works. [Here is a demonstration video.](http://youtu.be/4rcPfZveGZc)
Concept proven. This is definitely my favourite way so far. I will
probably write a simple algorithm in Emacs to format things on
separate lines, which would make it much easier to read, and I want to
make strings expand to fill the screen width, but no further. But this
is already an improvement.

I'll trial  it for a while, if I end up using
it more often than not, I'll make the option to make :present implicit
for all REPL evaluations.

## Example

For kicks, here's the output for

``` haskell
loeb (map (\i l -> Node i (map (fmap (+1)) l)) [1..3])
```

Normally you would get:

``` haskell
[Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest =
 [Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest =
  [Node {rootLabel = 5, subForest = [Node {rootLabel = 6, subForest =
   [Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest =
```

Ad infinitum! With presentation, you get:

``` haskell
λ> :present loeb (map (\i l -> Node i (map (fmap (+1)) l)) [1..3])
Tree Integer:[Tree Integer]
```

If you click `Tree Integer` on the left, you get:

``` haskell
(Node 1 [Tree Integer]):[Tree Integer]
```

Click the new one on the left:

``` haskell
(Node 1 (Tree Integer:[Tree Integer])):[Tree Integer]
```

Et cetera:

``` haskell
(Node 1 ((Node 2 [Tree Integer]):[Tree Integer])):
((Node 2 [Tree Integer]):[Tree Integer])
```

In other words, every `[Tree Integer]` is a placeholder that you can
click to get more output.
