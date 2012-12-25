---
date: 2010-10-14
title: Shime - Superior Haskell Interaction Mode for Emacs
author: Chris Done
tags: haskell, hakyll, shime
---

# Shimem nom nom nom

I have bare-faced stolen that name from
[SLIME](http://common-lisp.net/project/slime/), one of the best
programming environments available for any language. I originally
thought "inf-haskell" → "superior-haskell", but `M-x shime` is
nicer to type than `M-x superior-haskell`. The superiority of
Haskell is well-established, at any rate.

I decided I wasn't into
[inf-haskell](http://code.haskell.org/haskellmode-emacs/inf-haskell.el)
anymore. I like it but I want more features and a more
content-aware display (e.g. inf-haskell works on matching upon
the buffer, which works in most cases but often misses error
messages when scanning). I had a read through the
[Major Modes section of the Emacs manuals](http://www.gnu.org/s/emacs/manual/html_node/elisp/Major-Modes.html#Major-Modes)
and it was far easier than I thought to create a mode. There's a
lot of hullabaloo about the manual being spare, but go figure.

I made a start and have very basic REPL interaction working. I
made a Github repo:

  * [Shime Github repo](http://github.com/chrisdone/shime)

It's BSD licensed.

It talks to GHCi via a pipe. Observe:

    GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading package ffi-1.0 ... linking ... done.
    λ> 5 * 7
    35
    λ> :t 5 * 7
    5 * 7 :: (Num t) => t
    λ> "λx.x"
    "\955x.x"
    λ> putStrLn "λx.x"
    λx.x
    λ> :q
    Leaving GHCi.

*The Shime process died. Restart it? (y or n)* **y**

    GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading package ffi-1.0 ... linking ... done.
    λ>

***IT'S ALIVE!***

## Future plans

I intend on the following things (some of which I kind of
implemented in inf-haskell, but weren't very reliable):

* "Physical" error/warning messages -- ones that are collapsed
  and navigable with `TAB` and `Space` (or some combination that
  makes sense).
* Clickable types and variables; either displays definition or
  goes to definition, not sure yet.
* Some hardcore integration with Scion. Mostly for inspection.
* Support to keep certain modules loaded. Ever load some module,
  but get some simple type error, and you go to inspect a type
  from that module, but you can't because GHCi unloaded it? Me
  too*!*
* Defining of types inside GHCi: with interactive questioning
  this becomes less difficult, e.g. "Type A has dependencies,
  re-define it and re-evaluate its depending types? (y or n)",
  etc.
* Syntax highlighting.
* Multi-line expressions (by parsing and inserting semi colons).

Most probably I'll do about three of these and be satisfied, but
the intention is there.
