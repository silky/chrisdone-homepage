---
date: 2011-08-07
title: Flo: Generate flow diagrams from your codebase using annotations
description: Flo: Generate flow diagrams from your codebase using annotations
author: Chris Done
tags: haskell, flo
---

After discussing the idea with my colleague I thought it would be a
nice idea if you could annotate your source code in comments and from
that produce a flow diagram. I whipped up a proof of concept in
Haskell.

For an example [I've annotated the hpaste.org
codebase.](https://github.com/chrisdone/amelie/commit/d80da7c59514c5cbb279ea99c7c5921d9a2f3ec3)

[The output](http://i.imgur.com/Va8Xo.png) is quite nice. I produced
it with this command line:

    $ cd src
    $ find -name '*.hs' | xargs flo '-hs=-- @ ' | dot -Tpng > amelie.png

I uploaded it [to hackage](http://hackage.haskell.org/package/flo) and
I have [a Github project here](http://github.com/chrisdone/flo).

Give it a try.
