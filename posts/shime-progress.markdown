---
date: 2010-11-25
title: Shime progress and multiple sessions
description: Shime progress and multiple sessions
author: Chris Done
tags: haskell, emacs, shime
---

Recently I mentioned I started work on [a Haskell interaction mode for
Emacs called Shime.](http://chrisdone.com/posts/2010-10-14-shime-haskell-interaction-mode-for-emacs.html)
Since mentioning it I have basically achieved what I use from
inf-haskell, but with the addition of multiple sessions which turns
out to be very very cool.

I uploaded
[a video on YouTube a while ago demonstrating multiple sessions.](http://www.youtube.com/watch?v=Ly_T1hFGoXg)
I use this constantly. Right now I have two work project sessions
open, and one of my own personal projects, each with their own load
paths, Cabal configuation/build paths, and of course GHCi sessions. I
can kill/relaunch/compile them all in a natural way.

When I first open a Haskell file and hit `F5` (my binding for "load the
file in GHCi"), it asks me which session I want, I pick, e.g. "amelie"
and then it loads the file and remembers for the remainder of the
Emacs session. Actually, it only prompts if there is more than one
session open, otherwise it *Does What You Mean*.

Likewise when I hit `C-c C-c` it asks me which session I want to build
it with, using Cabal. I can use all the Cabal commands `configure`,
`build`, `install`, `upload`, etc. all from Emacs without thinking
about it. Pretty nice.

Sometimes you need to patch a library package that you're using. You
want to be able to load the files in GHCi to test your changes, and
then to build and install it, then rebuild your actual project and
test that. Shime makes that really easy.

Regarding the other features, they will be implemented when I really
want them. I did at least implement collapsed error messages (not
fully collapsed, just removing newlines -- turns out I rarely read
anything other than the type error or line number in error
messages). My REPL is now significantly more space efficient than it
was.
