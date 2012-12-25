---
date: 2010-09-26
title: Juicy juicy web feeds!
author: Chris Done
tags: blog, haskell, juicy
---

A while ago I made a Haskell web app called Juicy which downloads
feeds from community-voted aggregators and calculates how "juicy"
they are, i.e., how much movement there is and how much new
stuff comes in over time. It downloads the feeds once an hour.

I put it [on Github](http://github.com/chrisdone/juicy) and then
forgot about it. Today I found it again and fixed a bug that
baffled me a while ago -- an integer overflow on [threadDelay](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent.html#v%3AthreadDelay) --
and thought I'd put it online
[here](http://chrisdone.com/juicy/).  It should build a better
picture over time. Originally I wrote it to prove that
YCombinator is more fresh than Reddit. We'll see, I guess. I
don't visit either anymore.
