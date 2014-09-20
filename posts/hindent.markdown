---
date: 2014-09-20
title: hindent: A Haskell indenter
description: hindent: A Haskell indenter
author: Chris Done
tags: haskell
---

## A question of style

In this post I'm going to use the word “style” to refer to the way
code is printed in concrete terms. No changes in the code that would
yield a different syntax tree are considered “style” here.

## What's the deal with code style?

Code style is important! If you're a professional Haskell programmer,
you're working with Haskell code all day. The following things are
affected by the style used:

* How easily it can be manipulated with regular editors: the more code
  is laid out in a way that prevents you from readily using your
  normal editor functions, the less efficient you are.
* How well general tooling works with it: do diff and things like that
  work well?
* How easily you can absorb the structure: do you have to spend time
  hunting around for the start and end of syntactical nodes? Can't see
  the tree for the forest?
* How quickly you can write it: can you just write code or do you have
  to spend time thinking about how it's going to be laid out before
  writing, or editing the layout afterwards?
* How aesthetically offended you are[^1]: does the code you're looking
  at assault your sense of beauty?

Code style is important! Let's have a code style discussion. I propose
to solve it with tooling.

## Is this really an issue, though?

Okay, so I'm one guy with a bee in his bonnet. Let's do a quick Google
and see what
others are saying in
[this StackOverflow](http://stackoverflow.com/questions/1983047/good-haskell-coding-standards)
question:

> Could someone provide a link to a good coding standard for Haskell?
> I've found this and this, but they are far from comprehensive.

The following points refer to style:

* Format your code so it fits in 80 columns. (Advanced users may
  prefer 87 or 88; beyond that is pushing it.)
* Put spaces around infix operators. Put a space following each comma
  in a tuple literal.
* Prefer a space between a function and its argument, even if the
  argument is parenthesized.
* Use line breaks carefully. Line breaks can increase readability, but
  there is a trade-off: Your editor may display only 40–50 lines at
  once. If you need to read and understand a large function all at
  once, you mustn't overuse line breaks.
* When possible, align -- lines, = signs, and even parentheses and
  commas that occur in adjacent lines.

Even the Haskell community is not immune to
[long, protracted debates about tabs vs spaces.](http://www.reddit.com/r/haskell/comments/15gz8q/a_nondirty_shot_at_tabs_vs_spaces/)
That reddit submission has zero points. That means it's very
controversial. The submission also has 117 comments. That means people
are very vocal about this topic. That's because bike-shedding is
inversely proportional to the triviality of the debated thing. We
know that.

Nevertheless, style is important enough to be discussed.

## So let's formalise a standard style

That's a simplification. There are many style guides:

* [Caltech's style guide](http://courses.cms.caltech.edu/cs11/material/haskell/misc/haskell_style_guide.html)
* [Good Haskell Style](http://urchin.earth.li/~ian/style/haskell.html)
  (unattributed)
* [Johan Tibell's](https://github.com/tibbe/haskell-style-guide) style guide
* [My own style guide](https://github.com/chrisdone/haskell-style-guide)
* [Snap's style guide](http://snapframework.com/docs/style-guide)

These are just public ones. In-house styles are also common, for a
particular company. It's not practical to force everyone into one
single style. It's
[a well-worn topic in the C world](http://en.wikipedia.org/wiki/Indent_style#Styles).

## Okay, but is this a problem tooling even needs to solve?

There is precedent for other tooling based on style:

* [Neil Mitchell's hlint](http://community.haskell.org/~ndm/hlint/):
  “HLint (formerly Dr. Haskell) reads Haskell programs and suggests
  changes that hopefully make them easier to read.”
* [Doug Beardsley's hstyle](https://github.com/mightybyte/hstyle):
  “The ultimate goal would be an automatic reformatter that generates
  gorgeous code--but that's a hard problem.”
* The
  [previously mentioned Caltech style guide](http://courses.cms.caltech.edu/cs11/material/haskell/misc/haskell_style_guide.html)
  contains a program to check source code.
* That's not to mention [the venerable GNU indent](http://www.gnu.org/software/indent/).
* And more recently, [js-beautify](https://www.npmjs.org/package/js-beautify)

## Everyone has their own style

So then let's make a tool with a select number of styles, you might
say. The problem is that people don't even use the standards that
exist out there. They used slightly varying versions. Ask any
Haskeller what style they use, and they will say “mostly like X, but
with some differences.”

For example
[What are some good style guides for writing Haskell code?](http://www.quora.com/What-are-some-good-style-guides-for-writing-Haskell-code)[^2]

> I use a very similar style, with a few modifications. […] Perhaps I
> should write out my own style guide at some point, especially as
> I've become pretty set in my style lately.

And
[Good Haskell Coding Style](http://robertmassaioli.wordpress.com/2011/02/19/good-haskell-coding-style/)[^2]:

> My Personal Pet Peeves
>
> For the most part the style guides that I have added above (and the
> tools provided) mirror my own style guide (or perhaps my guide mirrors
> them). However, there is one item of style that particularly annoys me
> on a regular basis. […]

## Can't we just use structured editors?

Some more optimistic folk out there might suggest, perhaps, we should
just throw away text files and go straight for structured code
databases. Put all this formatting nonsense behind us. Layout is just
a stylesheet! It's not data to be stored in a file!

Maybe so. But nobody is using structured editors yet.

## A practical way forward

Taking all of the above into consideration, here is my approach at the
problem. The [hindent](https://github.com/chrisdone/hindent) library
and program. Styled on GNU indent, the intention is that you simply
run the program on some source code and it reformats it.

    $ hindent
    hindent: arguments: --style [fundamental|chris-done|johan-tibell]

hindent has the concept of styles built in. There is
[a type](http://chrisdone.github.io/hindent/HIndent-Types.html#t:Style)
for it:

``` haskell
data Style =
  forall s. Style {styleName :: !Text
                  ,styleAuthor :: !Text
                  ,styleDescription :: !Text
                  ,styleInitialState :: !s
                  ,styleExtenders :: ![Extender s]
                  ,styleDefConfig :: !Config
                  }
```

It contains authorship metadata. It holds an initial state which can
be used during printing. Most importantly, it has a list of
extenders. Means to extend the printer and change its behaviour on a
node-type-specific basis.

Take a normal pretty printing approach. It's usually something like:

``` haskell
class Pretty a where
  pretty :: a -> String
```

Then for all the types in
[the AST](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-Annotated-Syntax.html)
we implement an instance of `Pretty`.

The limitation here is that we can't, as a user of the library,
decide to print one particular node type differently.

Instead, here's a new class:

``` haskell
class (Annotated ast,Typeable ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()
```

(It runs in a `Printer` type to store some state about the current
column and indentation level, things like that.)

Now, we implement the `prettyInternal` method for all our types. But
when implementing instances, we never use the `prettyInternal` method
directly. Instead, we use another function, `pretty`, which can be
considered the main "API" for printing, like before. Here's the type:

``` haskell
pretty :: (Pretty ast) => ast NodeInfo -> Printer ()
```

We'll look at its implementation in a moment. Here is an example
instance of `Pretty`:

``` haskell
instance Pretty ClassDecl where
  prettyInternal x =
    case x of
      ClsTyDef _ this that ->
        do write "type "
           pretty this
           write " = "
           pretty that
      …
```

See how `pretty this` and `pretty that` are used for recursing instead
of `prettyInternal`? This approach is used for
[all instances](http://chrisdone.github.io/hindent/HIndent-Pretty.html#g:1).

Now let's see what that affords us:

``` haskell
pretty :: (Pretty ast) => ast NodeInfo -> Printer ()
pretty a =
  do st <- get
     case st of
       PrintState{psExtenders = es,psUserState = s} ->
         do case listToMaybe (mapMaybe (makePrinter s) es) of
              Just m -> m
              Nothing -> prettyNoExt a
            printComments a
  where makePrinter s (Extender f) =
          case cast a of
            Just v -> Just (f s v)
            Nothing -> Nothing
        makePrinter s (CatchAll f) = (f s a)
```

In this method, we're grabbing our (mentioned earlier) list of
`[Extender s]` values from `psExtenders` and then looking up to see if
any of the types match. To clarify, here is the `Extender` type:

``` haskell
data Extender s where
  Extender :: forall s a. (Typeable a) => (s -> a -> Printer ())
           -> Extender s
  CatchAll :: forall s. (forall a. Typeable a => s -> a -> Maybe (Printer ()))
           -> Extender s
```

Both constructors are rank-n. Both accept the current state as an
argument and the current node. The `Extender` constructor is
Prismesque. It's existential, and lets you say “I want things of this
type”. The `CatchAll` will just accept anything.

All that adds up to me being able to do something like this. Here's a
demo style:

``` haskell
demo =
  Style {styleName = "demo"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Demo for hindent."
        ,styleInitialState = ()
        ,styleExtenders =
           [Extender (\_ x ->
                        case x of
                          InfixApp _ a op b ->
                            do pretty a
                               pretty op
                               pretty b
                          _ -> prettyNoExt x)]
        ,styleDefConfig = def}
```

(The `prettyNoExt` is a publicly exposed version of the (private)
method `prettyInternal`.)

Now let's test the `fundamental` style versus our `demo` style:

``` haskell
λ> test fundamental "x = foo (a * b) + what"
x =
  foo
    (a * b) + what
λ> test demo "x = foo (a * b) + what"
x =
  foo
    (a*b)+what
```

Viola! We've configured how to pretty print infix operators in a few
lines of code.

In practice, there are three styles. Here's a larger example:

``` haskell
foo = do print "OK, go"; foo (foo bar) -- Yep.
          (if bar then bob else pif) (case mu {- cool -} zot of
            Just x -> return ();
            Nothing -> do putStrLn "yay"; return (1,2)) bill -- Etc
  where potato Cakes {} = 2 * x foo * bar / 5
```

Fundamental style:

``` haskell
foo =
  do print
       "OK, go"
     foo
       (foo
          bar)
       (if bar
           then bob
           else pif)
       (case mu {- cool -}
               zot of
          Just x ->
            return
              ()
          Nothing ->
            do putStrLn
                 "yay"
               return
                 (1,2))
       bill -- Etc
  where potato Cakes{} =
          2 * x
                foo * bar / 5
```

Johan Tibell's style (which I've only started implementing, there're
some things I need to clarify):

``` haskell
foo = do
    print "OK, go"
    foo
        (foo bar)
        (if bar
             then bob
             else pif)
        (case mu {- cool -} zot of
             Just x ->
                 return ()
             Nothing -> do
                 putStrLn "yay"
                 return (1, 2))
        bill -- Etc
  where
    potato Cakes{} =
        2 * x foo * bar / 5
```

My style (pretty much complete):

``` haskell
foo =
  do print "OK, go"
     foo (foo bar)
         (if bar
             then bob
             else pif)
         (case mu {- cool -} zot of
            Just x -> return ()
            Nothing ->
              do putStrLn "yay"
                 return (1,2))
         bill -- Etc
  where potato Cakes{} = 2 * x foo * bar / 5
```

The styles are part of the
[module hierarchy of the package](http://chrisdone.github.io/hindent/).

## Write your own style!

I welcome anyone to write their own style. All styles are based upon
the fundamental style, which should never change, by extending it. You
can base yours off of another style, or start from scratch. While you
can keep your style locally, like your XMonad configuration, it's
encouraged to contribute your style as a module.

My style and Johan's are quite different. But yours may be similar
with small tweaks. Another distinctly different style is Simon Peyton
Jones's with explicit braces. This is a style you can implement if you
want it.

See
[the contributing section](https://github.com/chrisdone/hindent#contributing-your-own-printer-style)
of the README for more info.

## Preserving meaning

A recommendation is to preserve the meaning of the code. Don't make
AST changes. Like removing parentheses, changing $ into parens, moving
lets into wheres, etc. You can do it, but the results might surprise
you.

## Editing advantages

Having implemented
[Emacs support](https://github.com/chrisdone/hindent#emacs), I have
been using this for a few weeks on my own code. It's amazing. I've all
but stopped manually making style changes. I just write code and then
hit `C-c i` and it almost always does exactly what I want.

It can't always do what I want. It has simple, predictable
heuristics. But even when it doesn't do what I want, I've so far been
okay with the results. The advantages vastly outweigh that.

## Remaining problems

I need to write a test suite for the fundamental style (and maybe the
others). This isn't hard, it's just a bit laborious so I haven't
gotten round to it yet.

There're some parts of the AST I haven't finished filling out. You can
[see them](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Pretty.hs)
which are marked by FIXME's. This just means if you try to format a
node type it doesn't know how to print, you'll get a message saying
so. Your code won't be touched.

Comment re-insertion is a little bit of a challenge. I have a decent
implementation that generally preserves comments well. There're
some corner cases that I've noticed, but I'm confident I have the
right change to make to clear that up.

The fundamental printer is fast. My personal `ChrisDone` style is
slower, due to its heuristics of deciding when to layout a certain
way. It took 6s to layout a complex and large declaration. I updated
my style and brought it down to 2s. That's okay for now. There are
always speed tricks that can be done.

There are of course issues like whether HSE can parse your code,
whether you have #ifdef CPP pragmas in your code, and things like
that. That's just part of the general problem space of tools like
this.

## Remaining ideas

Currently you can only reformat a declaration. I don't yet trust (any)
Haskell printer with my whole file. I invoke it on a per-declaration
basis and I see the result. That's good for me at the moment. But in
the future I might extend it to support whole modules.

Implement some operator-specific layouts. There are some operators
that can really only be laid out in a certain way. Control.Applicative
operators spring to mind:

``` haskell
Foo <$> foo
    <*> bar
    <*> mu
```

This can easily be handled as part of a style. Other considerations
might be strings of lens operators. I've noticed that people tend not
to put spaces around them, like:[^3]

``` haskell
foo^.bar.^mu.~blah
```

There's also alignment, which is another possibility and easily
implemented. The challenge will be deciding when alignment will look
good versus making the code too wide and whitespacey. In my own style
I personally haven't implemented any alignment as it's not that
important to me, but I might one day.

## Summary

Hopefully I've motivated the case that style is important, that
formalizing style is important, and that automating it is practical
and something we should solve and then move on, redirect our energy
that was wasted on manually laying things out and debating.

[^1]: Granted, this isn't a technical consideration. But it's a real
one.
[^2]: Both of these are referring to conventions other than simple
layout, but I don't think that detracts the point.
[^3]: I don't know lens's operators so that might not be real code, but the
point is that could be a style to implement.
