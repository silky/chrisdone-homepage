---
date: 2010-04-09
title: A bit of cabal-install hacking
description: A bit of cabal-install hacking
author: Chris Done
tags: haskell, cabal-install, cabal
---

**EDIT:** It appears cabal-install already supports bash completion
  for commands, provided by a script in the bash-completion directory
  of the cabal-install package. It also supports flag
  completion. Cool!

A couple other people at Zurihac agreed that it would be nice if Cabal
(cabal-install) supported completion like `git`. You can write `git
co` instead of `git commit`, etc. saving a bit of typing in the late
hacking hours. I made [a wee patch for Cabal 0.6.4 to do
this.](http://github.com/chrisdone/cabal-install/commit/cf74c1e9136b53ac198bc6915abd57cb9972ec2c)
Here’s me building cabal-install with cabal-install.

    chris@chrisamilo:~/Haskell/cabal-install-0.6.4$ cabal co;cabal b;cabal i
    Resolving dependencies...
    Configuring cabal-install-0.6.4...
    Preprocessing executables for cabal-install-0.6.4...
    Building cabal-install-0.6.4...
    Resolving dependencies...
    Configuring cabal-install-0.6.4...
    Preprocessing executables for cabal-install-0.6.4...
    Building cabal-install-0.6.4...
    Installing executable(s) in /home/chris/.cabal/bin

It’s pretty nice. I guess it’s probably preferable to write some
personal scripts to run on top of cabal but it’d be nice if everyone
had it.

As with most Haskell projects the source code is really easy to patch
so I’ll probably do more cabal-install hacking.
