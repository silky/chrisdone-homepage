---
date: 2010-06-13
title: Try Haskell Updates
description: Try Haskell Updates
author: Chris Done
tags: haskell, tryhaskell
---

In the past month I moved to Italy for a new Haskell job. Hopefully,
we will be releasing some open source contributions.

I added [about 25 more steps to Try Haskell and touched on types in
Haskell, clickable code
samples](http://github.com/chrisdone/tryhaskell/commits/master/),
lessons selection, go-back support,
stricter expression checking, from the help of feedback from
[YCombinator](http://news.ycombinator.com/item?id=1393593), including
[Simon Pratt](http://simondavidpratt.com/), who provided [various
patches to the jquery
console](http://github.com/chrisdone/jquery-console/commits/master/)
with refactoring, fixing the history and adding standard console
shortcuts.

I [tidied up the JSON service
code](http://github.com/chrisdone/haskell-json/commits/master/),
removed redundant files, tided up module imports, made the mueval-core
path not hard coded, etc. It is currently written with the FastCGI
API. I have started a branch with the aim of switching it to
[Snap](http://snapframework.com/).

I have also added top-level binding support, e.g.:

    > x = 1
    > (y,z) = (5,6)
    > x * y * z
    => 30
    >

It [works mostly in the same way GHCi does, with nested LET
expressions](http://github.com/chrisdone/haskell-json/commit/ee87544698759eadbb90a911b78343bbde8531a6). I
think this way is sound. The bindings are per-session; they expire
when you close your browser.

There is one change to the JSON service, when evaluating a top-level
expression, the success return value is:

    {"bind":"OK."}

Failure is returned as normal, e.g.

    {"error":"Not in scope: `y"}

Another neat thing I added as [suggested by tumult on
YCombinator](http://news.ycombinator.com/item?id=1393735) is support
to link to a given expression, e.g.:

    > map (*2) [1..1 0]
    => [2,4,6,8,10,12,14,16,18,20]
    > link
    link for map (*2) [1..10]
    >

(The [`link for map (*2)
[1..10]`](http://tryhaskell.org/?input=%6d%61%70%20%28%2a%32%29%20%5b%31%2e%2e%31%30%5d)
text is a link.)

I also [re-enabled Raphael
support](http://tryhaskell.org/?input=circle%2020%2020%2020) for the
hell of it.

I have more plans for it such as embedded code editor and smallcheck
exercises, user-uploaded tutorials, and adding an embedded console to
Hackage at some point would be nice, but that is all for now.

In other news, two other languages have started their own Try *Lang*
sites! Check these out:

* [Try Clojure](http://www.try-clojure.org/)
* [Try Erlang](http://www.tryerlang.org/)

Raising awareness of all functional languages is a great thing!
