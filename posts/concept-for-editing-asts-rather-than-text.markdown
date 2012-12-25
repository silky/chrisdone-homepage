---
date: 2011-12-17
title: A concept for editing code as an AST rather than text
description: A concept for editing code as an AST rather than text
author: Chris Done
tags: haskell
---

[Here is a demo video](http://www.youtube.com/watch?v=v2ypDcUM06U).

It's not my intention for this to be all point and click, at all, but
coming up with keybindings and a means to navigate the AST via
keyboard is an interesting problem somewhat separate to the problem of
creating/displaying/editing as-is. As you can see I'm even confused
myself when using it, and it's a lot slower with the mouse. With
keyboard control it could be blindingly faster than any text
editing-based language editors available now. Think paredit-mode, but
for non-Lisp languages.

I've had this idea in the back of my mind for years and today thought
that I might do a concept implementation to solidify the idea
somewhat.

The idea is that you can't create a syntactically invalid tree, and at
each point it can offer you a choice between the valid choices. That's
one part, the correctness. But that's merely a nice side-effect to the
idea of purely syntactical editing, rather than textual editing, so
that jumping around, transposing, moving, deleting expressions will be
a lot easier. Even so that there is no need to care about indentation,
but rather moving things about the AST. Still merely a concept at this
point. It's really hard to think about what you might like from an
editing mode like this without going ahead and implementing something.

It can technically be generalized to any programming language, but
Haskell is my main working language so I am targeting it specifically.

It could also be helpful for newbies, being guided on the
syntax. Purists will argue people should be able to write syntax. I
suppose they'd be right.

In summary I made a little DSL for describing an AST for manipulating
like this. There is a "list of things" combinator, an "optional"
combinator (e.g. the "module" decl is optional), there is a "choice"
combinator, e.g. when adding a new top-level decl it prompts for a
choice between the different types of decls, validating text inputs
(e.g. module name, constructor, variable, etc.), and that is more or
less enough, as far as I can tell, so far. Hopefully it won't get much
more complicated than that.

For a real implementation I would probably do it in Emacs, if overlays
would permit me enough power to do it (I think so). But it could also
be implemented in Yi, or Leksah, or Vim, or whatever, were those users
so inclined.

If you're implementing something like this already, I'd be interested
to see it. If you have some interesting ideas, feel free to comment.
