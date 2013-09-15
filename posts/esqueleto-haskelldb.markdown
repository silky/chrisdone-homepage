---
date: 2013-08-05
title: Comparison: Esqueleto from a HaskellDB perspective
description: A comparison of features between Esqueleto and HaskellDB
author: Chris Done
tags: haskell, haskelldb
---

Esqueleto looks pretty good, I'm considering using it as a modern
replacement for HaskellDB. From what I've been able to gather, it's
different in the following ways:

* Seems to be a monad, but I'm not sure in what flavour
* Doesn't have an extensible record system (instead you use tuples)

But does have the other good stuff of HaskellDB:

* Efficient joins
* Can query things like COUNT(*)
* Can do projection (good for not pulling the whole row into memory
  just to get one field)
* Can be extended with functions and values (e.g. postgresql's
  date/time functions, full text support, etc.)
* It's based on persistent so I presume it can do enums

And something that HaskellDB doesn't do:

* Using proper data types, so pattern matching can be used and such,
  rather than the HList approach in HDB

As an
[avid haskellDB user](http://chrisdone.com/posts/haskelldb-tutorial),
I'm serious about switching to esqueleto, so I sent the differences
that I garnered above to Felipe Lessa to document somewhere, so that
the next Haskeller doesn't have to figure out the difference.

**Update:** Felipe got back to me, linked me
  [this](http://blog.felipe.lessa.nom.br/?p=68), I'll summarize that
  and the above I wrote in a pull request for the package description
  later today.