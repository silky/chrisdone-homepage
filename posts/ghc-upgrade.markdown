---
date: 2012-01-22
title: Upgrading GHC
description: Upgrading GHC
author: Chris Done
tags: haskell, ghc
---

So this weekend I finally upgraded two large work projects from GHC
6.12.3! And I didn't upgrade one up in some gradual nonsense, I jumped
straight to GHC 7.2.2, the latest release, like a boss.

Why did I still have projects running on 6.12.3? GHC 6.12.3 is a two
year old release, and the codebases I upgraded are also two years
old. I have about 46 dependencies with specific constraints and some
patched dependencies, which made the prospect of updating quite
daunting. I didn't really *need* to upgrade, the software I was
writing worked.

Did I even try to uprade? Yes, I tried several times off-hand over the
past months to upgrade my GHC but was met immediately with hard to
grok Cabal complaints about version numbers. Not to mention some
dependencies that just didn't support the latest GHC. For a while I
had genuinely thought it would be hopeless to upgrade these projects
without tears.

Today I came back to it and decided that I'd start with an empty Cabal
file and add dependencies incrementally until every one was satisfied,
and then the project built. As far as GHC itself goes, separate from
Cabal, I only had to add `BangPatterns` and `FlexibleInstances`
to some files and it was happy.

Does this say anything about the stability of Haskell as a platform? I
think so, a little. In the end it's certainly
easier for maintenance to keep up to date with the latest packages,
and I think I will introduce this factor into the continuous
integration system for my projects.

It also says that despite all the new stuff happening in Haskell, code
is at least stable a few versions old. There were only a couple
packages that I couldn't use in GHC 6.12.3. It also says that GHC
6.12.3 was a very decent and stable release, having been using it in
production for two years with no problems.

Now I'm quite excited about all the GHC features I've been missing out
on that I'll be able to start using. I have some reading to do, so
I've opened up the release notes of 7.0.1, 7.0.2, 7.0.3, 7.0.4, 7.2.1,
and 7.2.2, and [the release notes of the upcoming
7.4](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-December/021310.html). I
should also be able to try out the Scion package now, which is rather
awesome.
