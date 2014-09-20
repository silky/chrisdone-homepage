---
date: 2014-09-20
title: Formatting in Haskell
description: Formatting library
author: Chris Done
tags: haskell
---

This post is about the
[formatting](http://hackage.haskell.org/package/formatting) package.

## What's wrong with printf?

The `Text.Printf` module is problematic simply because it's not
type-safe:

``` haskell
λ> import Text.Printf
λ> printf "" 2
*** Exception: printf: formatting string ended prematurely
λ> printf "%s" 2
*** Exception: printf: bad formatting char 's'
```

And it's not extensible in the argument type. The
[PrintfType](https://hackage.haskell.org/package/base-4.7.0.1/docs/Text-Printf.html#t:PrintfType)
class does not export its methods.

And it's not extensible in the formatting. You can't add a "%K" syntax
to it to print a value in Kelvins, for example.

And it's implicit. You can't just use your normal API searching
facilities to search how to print a `Day`.

## Holy Moly!

A while ago I was inspired by the
[HoleyMonoid](http://hackage.haskell.org/package/HoleyMonoid) package
to use that mechanism to make a general replacement for
[`printf`](https://hackage.haskell.org/package/base-4.7.0.1/docs/Text-Printf.html).

It's a continuation-based way of building up monoidal functions by
composition with the ability to insert constants in-between. Example:

``` haskell
let holey = now "x = "
          . later show
          . now ", y = "
          . later show

> run holey 3 5
"x = 3, y = 5"
```

The `now` function inserts a monoidal value directly into the
composition. So

``` haskell
run (now x . now y)
```

is equivalent to

``` haskell
x <> y
```

And

``` haskell
run (later show . now x . later show . now y)
```

is equivalent to

``` haskell
\a b -> show a <> x <> show b <> y
```

## The Formatting package

The package is available on Hackage as
[formatting](http://hackage.haskell.org/package/formatting).

### Comparison with other formatters

Example:

``` haskell
format ("Person's name is " % text %  ", age is " % hex) "Dave" 54
```

or with short-names:

``` haskell
format ("Person's name is " % t % ", age is " % x) "Dave" 54
```

Similar to C's `printf`:

``` c
printf("Person's name is %s, age is %x","Dave",54);
```

and Common Lisp's `FORMAT`:

``` lisp
(format nil "Person's name is ~a, age is ~x" "Dave" 54)
```

### The Holey type

``` haskell
newtype HoleyT r a m = Holey { runHM :: (m -> r) -> a }

type Holey m r a = HoleyT r a m
```

This is my version of the `HoleyMonoid`. To make this into a useful
package I changed a few things.

The `Category` instance implied a name conflict burden with `(.)`, so I
changed that to `(%)`:

``` haskell
(%) :: Monoid n => Holey n b c -> Holey n b1 b -> Holey n b1 c
```

Rather than have the name-conflicting `map` function, I flipped the
type arguments of the type and made it an instance of `Functor`.

### Printers

There is an array of top-level printing functions for various output
types:

``` haskell
-- | Run the formatter and return a lazy 'Text' value.
format :: Holey Builder Text a -> a

-- | Run the formatter and return a strict 'S.Text' value.
sformat :: Holey Builder S.Text a -> a

-- | Run the formatter and return a 'Builder' value.
bprint :: Holey Builder Builder a -> a

-- | Run the formatter and print out the text to stdout.
fprint :: Holey Builder (IO ()) a -> a

-- | Run the formatter and put the output onto the given 'Handle'.
hprint :: Handle -> Holey Builder (IO ()) a -> a
```

All the combinators work on a lazy text
[Builder](http://hackage.haskell.org/package/text-1.1.1.3/docs/Data-Text-Lazy-Builder.html)
which has good appending complexity and can output to a handle in chunks.

### The Format type

There is a short-hand type for any formatter:

``` haskell
type Format a = forall r. Holey Builder r (a -> r)
```

All formatters are written in terms of `now` or `later`.

### Formatters

There is a standard set of formatters in
[Formatting.Formatters](http://hackage.haskell.org/package/formatting-5.2/docs/Formatting-Formatters.html),
for example:

``` haskell
text :: Format Text
int :: Integral a => Format a
sci :: Format Scientific
hex :: Integral a => Format a
```

Finally, there is a general `build` function that will build anything
that is an instance of the `Build` class from the `text-format`
package:

``` haskell
build :: Buildable a => Format a
```

For which there are a bunch of instances. See
[the README](https://github.com/chrisdone/formatting#hello-world-texts)
for a full set of examples.

### Composing formatters

`%.` is like `%` but feeds one formatter into another:

``` haskell
λ> format (left 2 '0' %. hex) 10
"0a"
```

## Extension

You can include things verbatim in the formatter:

``` haskell
> format (now "This is printed now.")
"This is printed now."
```

Although with `OverloadedStrings` you can just use string literals:

``` haskell
> format "This is printed now."
"This is printed now."
```

You can handle things later which makes the formatter accept arguments:

``` haskell
> format (later (const "This is printed later.")) ()
"This is printed later."
```

The type of the function passed to `later` should return an instance
of `Monoid`.

``` haskell
later :: (a -> m) -> Holey m r (a -> r)
```

The function you format with (`format`, `bprint`, etc.)
will determine the monoid of choice. In the case of this library, the
top-level formating functions expect you to build a text `Builder`:

``` haskell
format :: Holey Builder Text a -> a
```

Because builders are efficient generators.

So in this case we will be expected to produce Builders from arguments:

``` haskell
format . later :: (a -> Builder) -> a -> Text
```

To do that for common types you can just re-use the formatting library
and use bprint:

``` haskell
 > :t bprint
bprint :: Holey Builder Builder a -> a
> :t bprint int 23
bprint int 23 :: Builder
```

Coming back to `later`, we can now use it to build our own printer
combinators:

``` haskell
> let mint = later (maybe "" (bprint int))
> :t mint
mint :: Holey Builder r (Maybe Integer -> r)
```

Now `mint` is a formatter to show `Maybe Integer`:

``` haskell
> format mint (readMaybe "23")
"23"
> format mint (readMaybe "foo")
""
```

Although a better, more general combinator might be:

``` haskell
> let mfmt x f = later (maybe x (bprint f))
```

Now you can use it to maybe format things:

``` haskell
> format (mfmt "Nope!" int) (readMaybe "foo")
"Nope!"
```

## Retrospective

I've been using `formatting` in a bunch of projects since writing
it. Happily, its API has been stable since releasing with some
additions.

It has the same advantages as `Parsec`. It's a combinator-based
mini-language with all the same benefits.
