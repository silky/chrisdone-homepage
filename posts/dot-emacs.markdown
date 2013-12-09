---
date: 2013-12-09
title: My Emacs configuration
description: Emacs
author: Chris Done
tags: emacs
---

Here's how to use
[my Emacs configuration](https://github.com/chrisdone/chrisdone-emacs),
which is optimized for Haskell development.

## Requirements

You need:

* Emacs
* Git
* hasktags

Everything else is either in my repo or pulled from git
submodules.

## Downloading

Clone it from github, pull the submodules and setup the
Haskell-specific tools.

    $ git clone https://github.com/chrisdone/chrisdone-emacs.git
    $ git submodule init
    $ git submodule update
    $ cd packages/haskell-mode; make; cd ../..
    $ cd packages/structured-haskell-mode; cabal install
    $ cabal install hasktags

## Tweaks

There are some strange things about my configuration that some users
might not like:

* You probably want to comment out
[this line](https://github.com/chrisdone/chrisdone-emacs/blob/master/init.el#L62)
about [`god-mode`](http://chrisdone.com/posts/god-mode)[^2].
* If you like arrow keys for code navigation, remove
[these lines](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/global.el#L145).[^1]
* If you don't like the dark theme, comment out `(zenburn)` on [this line](https://github.com/chrisdone/chrisdone-emacs/blob/master/init.el#L61).
* Keybindings that are different or additional to normal Emacs are
[here](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/global.el#L133).
* My haskell-specific keybindings are
[here](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el#L84).

Open a .hs file in a project and run C-` to start GHCi. Things should
be mostly self-explanatory from there. Look at the keybindings and
play around.

[^1]: I use `C-n/C-p/C-f/C-b` for navigation, which in `god-mode` is `npfb`.
[^2]: That enables a strange input style like Vim to avoid having to hit Ctrl.
