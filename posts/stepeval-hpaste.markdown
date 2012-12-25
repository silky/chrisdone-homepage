---
date: 2011-07-02
title: Step-wise evaluation of simple Haskell code to hpaste (via Stepeval)
description: Step-wise evaluation of simple Haskell code to hpaste (via Stepeval)
author: Chris Done
tags: hpaste, haskell, stepeval, amelie
---

I recently discovered
[stepeval](https://github.com/benmachine/stepeval), a program (and now
library since I patched it) written by
[Ben Millwood](https://github.com/benmachine) for evaluating simple
Haskell expressions step-wise, from a simple
Prelude[^1]. [In my fork](https://github.com/chrisdone/stepeval), I
merely
[add a Library interface and rename the hierarchy](https://github.com/chrisdone/stepeval/commit/efe3f7de2664f080d7c8e7edced32d25a09cebe8)
to `Language.Haskell.Stepeval`. See
[the example web service of stepeval here](http://www.srcf.ucam.org/~bm380/cgi-bin/stepeval.cgi). For
example, try `tail (map (const 'z') "ab")`.

I thought this library was a really neat idea. Especially given that
I'd recently read in
[The Risks and Benefits of Teaching Purely Functional Programming in First Year](http://www.cse.unsw.edu.au/~chak/papers/CK02a.html),
the claim that being able to evaluate Haskell code stepwise is a
useful teaching tool following nicely into equational reasoning,
correctness proof and derivation.[^2]

I added it to [hpaste](http://hpaste.org/),
[here is an example.](http://hpaste.org/steps/48627?expr=droprev+%22ab%22&submit=Submit)
I added [an "about" page](http://hpaste.org/stepeval) to explain what
library I'm using and display the current Prelude. Any Haskell paste
will display a "Steps" link at the top right, which allows one to run
an expression given the source paste. Another nice example a la
[SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_thm_1.10)
order of growth demonstration is
[ackermann](http://hpaste.org/steps/48629?expr=a+1+2&submit=Submit).

[^1]: [The README of the stepeval Github project explains](https://github.com/benmachine/stepeval):
“stepeval is a tool that can operate as a command-line utility or CGI
script.  In either case, it is given a Haskell expression in string
form, applies a single evaluation operation (e.g. pattern matching,
lambda application), and prints the result, while also feeding it back
into its evaluation mechanism until the expression cannot be evaluated
any further, or in the case of the CGI script a time limit has
expired.”

[^2]: Manuel M. T. Chakravarty and Gabriele Keller write, p4: “As
already mentioned, the clean semantics of functional languages leads
to a good integration of the teaching of programming techniques with
computing concepts and theory. For example, we encourage students from
the start to get a feeling for what a program does by way of stepwise
evaluation of expressions on a piece of paper. This neatly provides a
starting point for the introduction of equational reasoning by
performing stepwise evaluation on expressions that are not closed,
which brings us to correctness proofs and program derivation. In our
opinion, this is signicantly [sic] easier to motivate and implement
than the calculus of weakest preconditions or the Hoare calculus that
would be the corresponding theory for imperative languages.”
