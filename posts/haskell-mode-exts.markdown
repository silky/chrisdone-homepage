---
date: 2010-08-11
title: Haskell mode Emacs extensions
author: Chris Done
tags: haskell, emacs, yi
---

I have made [a github project for a package of Emacs modules for
extending Haskell mode.](http://github.com/chrisdone/haskell-mode-exts)
Some initial modules so far:

1. haskell-align-imports.el
2. haskell-installed-packages.el
3. haskell-navigate-imports.el
4. haskell-sort-imports.el
5. inf-haskell-send-cmd.el

I have more in progress and more planned.

One example is
[making editing imports automatically alphabetically ordered and indented.](http://www.youtube.com/watch?v=UXHSiUPKLvg&fmt=35)
This one is a little tricky to get right so I will get back to you
on this one. Once I am happy with it, I will generalize it to a
lot of Haskell statements, like `data` declarations:

    data Foo = X
             | Y

If I type `bar` after `Foo`, I should get,

    data Foobar = X
                | Y

automatically. I have seen this type of thing in [the Yi editor](http://haskell.org/haskellwiki/Yi) and
it is a really good idea. Equally for definitions:

    foo x = do x
               y

If I add another parameter, the rest of the function body should
be indented accordingly,

    foo x y = do x
                 y

and so on.
