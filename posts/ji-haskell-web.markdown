---
date: 2011-12-26
title: Ji, a little library for controlling a web browser from Haskell
description: Ji, a little library for controlling a web browser from Haskell
author: Chris Done
tags: haskell, web, javascript
---

As of recent I have only been creating a lot of new projects, not
working one existing ones or finishing half done ones off.[^1]

So here's yet another little project that is to test the concept of
controlling the web browser's DOM from Haskell as a means to write
user applications.

It doesn't use websockets as websockets aren't well supported[^2], so I
just used a simple poll/push protocol.

The code is on Github:

* [https://github.com/chrisdone/ji](https://github.com/chrisdone/ji)

Haddock docs
[here](http://chrisdone.com/ji/doc/html/ji/Graphics-UI-Ji.html).

Here are two examples I made so far:

* [Some simple buttons](http://chrisdone.com/ji/buttons/)
* [Missing dollars question](http://chrisdone.com/ji/missing-dollars/)

It seems fairly viable so far. I would have liked to produce many more
examples, but I couldn't really think of any. I stole the idea for the
dollars from Albert Lai. There is more room for optimizations, but
until I do a more large scale test, hard to say exactly where needs
it.

It might be a good test to rewrite TryHaskell with it. Probably still
too easy. I'll give it a while to think about it.

It could be a base on which to build a more high-level library or
framework.

I could also write a back-end for digestive-formlets.

[^1]: Partly this is a way to feel like I've spent my time well as
      it's easier to complete something small, and partly that ideas I
      get which sound feasible typically linger in my head asking to
      be prototyped, so this is a way of flushing them out.

[^2]: I wanted to use socket.io but there is no Haskell socket.Io
      back-end, and I didn't feel like writing Node. There are several
      websockets Haskell libraries, but as mentioned websockets itself
      isn't well supported, I'd have to upgrade to try it (and so
      would everyone else). Websockets would be the eventual protocol,
      though.
