---
date: 2013-12-09
title: structured-haskell-mode
description: structured-haskell-mode
author: Chris Done
tags: emacs, haskell
---

For about 2 months I've been working on and off on an Emacs package
called
[structured-haskell-mode.](https://github.com/chrisdone/structured-haskell-mode)[^2]
A full explanation and demo of the features is available on the Github
page. In summary, it is a mode that offers [paredit-mode](https://www.youtube.com/watch?v=D6h5dFyyUX0)[^5] abilities for
Haskell code.

I've been keeping it to myself in a private Github repo,
hoping to finish fleshing out the feature set[^1] and smooth over
stability issues. In the end I decided I'd put it out there, because
the base functionality is quite reliable and enough to get work done
better. It does actually change how you work.

The key features that enable new ways of working are:

1. Cutting and pasting actually preserves the indentation of the
   particular syntactic node. One doesn't have to think or care about
   “re-indenting” or worry about how much nesting is happening for
   fear of having to clean it up later. It's so trivial now.
2. Typing characters or removing them will “bring along” dependent
   node source, meaning that re-indentation is handled
   automatically. This means that you can use a nice consistent Lisp
   style[^3] without caring about how you're going to have to
   manually re-indent it whenever you make changes.
3. You now don't have to think about indentation. You think about
   nesting level. To go to the right place, you use the `)` keybinding
   to go further outwards in the node layers, and hit `C-j` to start a
   new sibling at that node level. There is no “tab cycle”. This style
   is 100% reliable.
4. Context-awareness is useful. In strings, the quote character is
   escaped. When hitting `C-j` in a list (of values, or types in a
   record, or a list of constructors in a data declaration), it can
   automatically add delimiter characters properly indented and spaced
   out. Something you don't want to have to care about doing yourself.
5. Parentheses are actually good. The Haskell tendency to abuse `$` to
   avoid having to manage parentheses is symptomatic of having crappy
   editing facilities. Managing parentheses in Haskell code is a pain,
   because editors don't know about things like Haskell's case
   expressions, or lambdas, or patterns, or whatever, and
   re-indentation is a nightmare inside parentheses. Not in this
   mode. Parentheses make editing a triviality rather than a chore.

The overarching theme to this whole library is to remove redundancy in
your work. Stop thinking so much about layout and syntactic debt[^4]
and appealing to the status quo[^6], and start just thinking about the
real work you're doing, which is plugging together programming
constructs.

[^1]: Stealing ideas from paredit-mode (e.g. slurping, barfing,
convoluting) and coming up with my own ideas, such as operand
manipulation, automagic re-indentation.

[^2]: It is actually a rewrite of a package I wrote six months ago of the
same name. That package was stable, but the code was not favourable
and there were some kinks to be ironed out. The new version uses Emacs
markers so structured operations fail less often.

[^3]: Lisp style is:

        foo bar
            mu
            (zot bar
                 bob)

[^4]: Syntactic debt is the energy and time you spend later on for
making decisions or choices now. Feel like you're nesting your
function too deep? Better stop now or you'll pay for it later because
you'll have to come back and collapse it down to fit within 80/120
columns! That's a real problem when your editor sucks. When you have
much better control over your code, things like that are a
non-issue. Just write the code, worry about layout when you're
done. Lispers know this.

[^5]: Emacs users who've written their share of Elisp will know that
paredit-mode is among the most enjoyable editing experiences out
there. Strangers to this editing experience are simply missing out on
the cream of the crop.

[^6]: The status quo has to debunked incrementally, I think. The next
thing to sort out is diffs. People waste their time making their code
more friendly to diff engines that only know about lines. Diffs should
be smart enough to know better. Expect further development in this
area.
