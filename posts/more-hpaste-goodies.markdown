---
date: 2010-09-25
title: More hpaste goodies
author: Chris Done
tags: blog, haskell, hpaste
---

It's been ten days since launching the new
[hpaste.org](http://hpaste.org/) and it seems to be successful. I
completed the following:

* Import old pastes from old web site
* Add a simple spam trap, which seems to be doing a good job
* Add a [HLint](http://community.haskell.org/~ndm/hlint/) hints feature
* Added [Codepad.org](http://codepad.org/) run output feature

[An example of both HLint and Codepad.org](http://hpaste.org/40093/xmonadhs). Remember:
To run the code (in Haskell at least) it needs a main function. I
will later add a bit of code to detect whether the code is
parseable (valid syntax) and runnable (includes main function)
automatically with
[haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts).

As of this post, Codepad.org supports the following languages: C,
C++, D, Haskell, Lua, OCaml, PHP, Perl, Plain Text, Python, Ruby,
Scheme, and Tcl. I wrote the code as a separate library that you
can use. It's on
[the Amelie project on Github](http://github.com/chrisdone/amelie/blob/master/src/Web/Codepad.hs)
for now, I also put it
[on hpaste as a more permanent place.](http://hpaste.org/40120/webcodepad)
Not sure if it's worth making as a Hackage package.

I still need to add the
[Context in IRC](http://bc.tech.coop/blog/041020.html) feature
that paste.lisp.org used to support and an RSS feed, and a
[tryhaskell](http://tryhaskell.org/) prompt.

After making the title and author fields mandatory, there's a big
difference in naming:
[here's the old hpaste.org pastes](http://i.imgur.com/qmsKR.png)
and
[here's the new ones since launching](http://i.imgur.com/ftYWL.png).

The amelie gitcast is still in the pipeline.

Drop me an email if you have interesting, do-able ideas for hpaste.

For those interested, here's
[a Google Analytics report for tryhaskell.org for the last month.](http://chrisdone.com/docs/GA_20100826-20100925.pdf)
I am curious about how many of those goal conversions actually
encourage anyone to look at a Haskell book or download the
compiler.
