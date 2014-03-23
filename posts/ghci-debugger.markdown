---
date: 2014-03-23
title: Emacs support for debugging Haskell
description: Emacs support for debugging Haskell
author: Chris Done
tags: haskell, emacs
---

One odd thing about the Haskell community is that we don't use
debuggers. We don't even use stack traces. I think for several
reasons:

1. Haskell code goes wrong less often.
2. Due to that, people are content with once in a while sticking in
   printf statements.
3. Lazy evaluation is known to make debugging tricky.
4. Haskell has no decent editor support for debugging.

It's at least my experience that when my Haskell code goes wrong, I
don't fret too much because I put my code into small functions. If
something's not working as expected then I run that function in GHCi
with typical inputs and check that the result is as desired.

If I'm in a codebase that makes it hard to do that, then I'll insert
some `error` or `trace` calls and re-run the whole program.

It's also my experience that I don't care about GHCi's debugging
support if I have to manually set breakpoints myself and step through
things manually. Who wants to bother doing that?

So, I thought, as an experiment, I'd whip up a simple interface to
GHCi's debugging facilities, based upon my (limited) understanding
from the manual and the tutorials about it, to help me answer the
following questions:

1. Is GHCi's debugger any good? I.e. it's useful, not quirky or obtuse.
2. Is it practical? I.e. it works on real project code.
3. Is lazy evaluation as problematic as suspected for real code?
4. Does Haskell lend itself to debugging with GHCi enough that I'd
   reach for it as part of my problem-solving workflow?

By removing the "it's hard to use" impedance from the debugger, that
puts me in a good position to evaluate the above questions. Is it that
we as a community are missing out on sweet tooling because we just
don't have a convenient way to use it?

[Here is a video](https://www.youtube.com/watch?v=ugWDkA6yReY)
demonstrating a trivial usage of it, i.e. the one that I've been
testing with. I invite overenthusiastic early adopters to try pulling
from the haskell-mode repo to play around with it and patch up obvious
deficiencies. You have to be using haskell-interactive-mode's REPL,
`(require 'haskell-debug)` and then run `M-x haskell-debug` from a
Haskell file or the REPL, like I do in the video. The rest should be
fairly obvious from the buffer's helpful messages.

I'm completely ambivalent about whether a debugger is really useful,
I've never really used one properly. So I'll try to use it to solve
some problems—once I can find some decent use-cases—and report back
in a few weeks.
