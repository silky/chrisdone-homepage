---
date: 2012-02-16
title: If expressions in Haskell-Emacs
description: If expressions in Haskell-Emacs
author: Chris Done
tags:  haskell, emacs
---

A little treat for myself an users of Haskell-Emacs, `if` expression
completion. It's just a small addition, but can be nice. There is way
more to come regarding structural editing which will make all this
textual editing business obsolete, but that's for later.

Suppose you're at the start of a statement and you want to write an if-expression:

<p><img src="/images/if1.png"/></p>

Now you can type “if ” and it'll fill one in and put you at the
condition point. If there's nothing before the current expression, it
assumes it's okay to use layout and not include parentheses.

<p><img src="/images/if2.png"/></p>

But what if you're in between some expressions, what gets inserted
then? Surely I don't want it using layout between two expressions,
that'd be odd.

<p><img src="/images/if3.png"/></p>

I decided if there's something after the current point, it just writes
the if on the same line.

<p><img src="/images/if4.png"/></p>

If you want, you can make sure it uses one line by
hitting `(' to insert two parens, and then type “if ” inside.

<p><img src="/images/if6.png"/></p>

And then it puts it on one line:

<p><img src="/images/if7.png"/></p>

If in doubt about how to trigger layout rather than parens, make sure
the if is the first thing on the line. So you'll not get this:

<p><img src="/images/if8.png"/></p>

But instead get what you want, which is this:

<p><img src="/images/if9.png"/></p>

And if all that isn't your cup of tea, you can disable it in `hs-config.el`

    (defvar hs-config-clever-ifs nil)
