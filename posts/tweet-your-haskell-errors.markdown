---
date: 2010-04-04
title: Tweet your Haskell type errors from Emacs
description: Tweet your Haskell type errors from Emacs
author: Chris Done
tags: twitter, haskell, emacs
---

## Tweet your failures!

I wrote [an Elisp script to send your GHCi type errors to
Twitter.](/code/haskell-tweet-errors.el)

![Twitter showing my type errors](/images/tweet-haskell-errors.png)

Add a little something like this to your .emacs:

    ;; (add-hook 'haskell-mode-hook
    ;;           '(lambda ()
    ;;              (load "haskell-tweet-errors.el")
    ;;              (define-key haskell-mode-map [f5]
    ;;                (lambda ()
    ;;                  (interactive)
    ;;                  (inferior-haskell-load-file-tweet-errors
    ;;                   "<username>" "<password>")))))

Hit F5 to load your Haskell file with Emacs into GHCi. If there’s a
type/kind/class error, it will twitter it and present you with a
message:

> Tweeted. Now the world knows about your type error!

## Why?

It’s something called intellectual honesty! According to a talk I saw
by Simon Peyton-Jones, the early Haskell compilers would delete your
source code upon a type error.

I figure why not replace mild annoyance with public shame?
