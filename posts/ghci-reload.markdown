---
date: 2014-03-16
title: Reloading running code in GHCi
description: Reloading running code in GHCi
author: Chris Done
tags: haskell, ghci
---

Something I've been using over the past couple weeks in a personal
Yesod web site is a way to reload the code while the server is still
running in GHCi. I saw in
[Greg Weber's](http://www.yesodweb.com/blog/2014/03/gsoc-proposals)
blog post about a "reload mode" for web servers and thought I'd share
my approach. GHCi already supports reloading of code, it just doesn't
know it.

The problem with doing this in GHCi is always that the `:load` and
`:reload` commands will clear out any bindings made in the REPL. This
means that even if you start your web server in a separate thread—and
it will stay running between reloads—you have no way to *update* or
*talk* to it directly.

That's why I wrote a package called
[foreign-store](http://hackage.haskell.org/package/foreign-store). Its
purpose is to make a stable pointer to some Haskell value and store it
at an index, and then keep hold of it in C. Later, it can provide that
stable pointer by that index. That's its whole purpose. Because the C
code is unaffected by GHCi's reloads, the pointers are retained, and
they are not garbage collected, because that is the point of a stable
pointer.

Now, with that created, it's possible to run a web server, keep hold
of the thread id, reload some code in GHCi, kill that thread and
restart it. Another option is to keep an `IORef` of the web handler
itself, and then update the `IORef` instead. In my use of it so far,
this has worked flawlessly.

I made [a demo project](https://github.com/chrisdone/ghci-reload-demo)
with a README explaining the (simple) approach.  The short of it is
that I can make some change to a Haskell module in my web project, hit
a key (F12), and instantaneously see the browser page refresh with the
new update. This is pretty much optimal for me.

It doesn't end at web servers, of course. Any kind of long-running
program that you would like to keep running while developing is fair
game. For example, an IRC server. Why not run the server and also
inspect the innards of its state while it's running, and also update
the message handler?  I've done this with my
[Hulk](https://github.com/chrisdone/hulk) IRC server before. You can
inspect the data structures, query the types of things, etc. all from
the REPL.[^1]

If you want to get really funky, you can try using the continuation
monad to implement
[Common Lisp's restarts.](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html)
Restarts are especially handy for when you're running some long IO
process and it bails out. You want to be able to correct the code and
the continue from where you left off. Restarts let you do that.

I shouldn't have to tell anyone this but just in case: _don't use this
in production_.

[^1]: Of course, there aren't many of us Haskellers who live in
      the REPL like Smalltalkers and Lispers do. Many Haskellers never
      even launch GHCi while developing.
