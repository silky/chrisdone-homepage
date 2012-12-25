---
date: 2010-11-25
title: Lisk - Lisp and Haskell
description: Lisk - Lisp and Haskell
author: Chris Done
tags: haskell, hakyll, shime
---

In my spare time I'm working on a project called Lisk. Using the
`-pgmF` option for GHC, you can provide GHC a program name that is
called to preprocess the file before GHC compiles it. It also works in
GHCi and imports. You use it like this:

    {-# OPTIONS -F -pgmF lisk #-}
    (module fibs
      (import system.environment)

      (:: main (io ()))
      (= main (>>= get-args (. print fib read head)))

      (:: test (-> :string (, :int :string)))
      (= test (, 1))

      (:: fib (-> :int :int))
      (= fib 0 0)
      (= fib 1 1)
      (= fib n (+ (fib (- n 1))
                  (fib (- n 2)))))

The git repo is here:

    git://github.com/chrisdone/lisk.git

And the [github page is here.](https://github.com/chrisdone/lisk)

I literally only support what is exhibited in the example above, and
it is not ready for use at all. But I am using
[haskell-src-exts's](http://hackage.haskell.org/package/haskell-src-exts)
AST and pretty printer in order to convert from Lisk to Haskell, so
I'm in good hands regarding completeness of the syntax. I don't have a
lot of time to work on it right now, but I will be doing.
