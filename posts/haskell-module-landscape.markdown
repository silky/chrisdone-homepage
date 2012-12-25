---
date: 2010-07-04
title: The Haskell module landscape
author: Chris Done
tags: haskell, hackage
---

I was pondering how naming Haskell packages with a proper hierarchy
is quite important, and wondered what the general hierarchy of the
Hackage packages was like. So I grabbed the [tarball of pakage
descriptions](http://hackage.haskell.org/packages/archive/00-index.tar.gz)
from [Hackage](http://hackage.haskell.org/packages/hackage.html) and
ran [a little messy Haskell script](http://gist.github.com/463472) and
got [this
list.](http://gist.github.com/raw/463423/f8458d83b1a7cc26cdbf812747188993e50cd8a2/The%20Haskell%20module%20landscape)

Here is a snippet:

    hsdns:          ADNS
    hsdns:          ADNS.Base
    hsdns:          ADNS.Endian
    hsdns:          ADNS.Resolver
    cv-combinators: AI.CV.ImageProcessors
    HOpenCV:        AI.CV.OpenCV.CV
    HOpenCV:        AI.CV.OpenCV.CxCore
    HOpenCV:        AI.CV.OpenCV.HighGui

It is interesting to see how there are quite a few packages that could
be easily put into the proper namesace, e.g ADNS below should be in
the Network hierarchy. It is also cool to see clusters of separate
packages providing functionality for the same module, e.g check out
Control.Applicative!

    special-functors:                 Control.Applicative
    base:                             Control.Applicative
    applicative-extras:               Control.Applicative.Backwards
    applicative-extras:               Control.Applicative.Compose
    applicative-extras:               Control.Applicative.Error
    InfixApplicative:                 Control.Applicative.Infix
    category-extras:                  Control.Applicative.Parameterized
    action-permutations:              Control.Applicative.Permutation
    applicative-extras:               Control.Applicative.State
    yjtools:                          Control.Applicative.Tools
    unicode-symbols:                  Control.Applicative.Unicode
    base-unicode-symbols:             Control.Applicative.Unicode

Control.Concurrent also has a lot going on inside it. There are quite a
lot of packages under Network, too.

One idea: Hackage could have an additional page which displays this,
fully linked up to the packages in question, with an MSDN-style
complete collapsible/expandable module hierarchy menu on the left and
documentation on the right. I am not saying that is the right way to do
it, but it certainly makes for fun browsing for the Haskell entire open
source codebase. (It would also encourage library writers to put their
libraries in a useful namespace.)
