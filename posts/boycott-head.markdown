---
date: 2011-10-17
title: Deprecate Prelude.head and partial functions
description: Deprecate Prelude.head and partial functions
author: Chris Done
tags: haskell
---

Please boycott
[Prelude.head](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:head)
and all its partial friends.

I'm sick of seeing [this exception](http://www.google.com/search?sourceid=chrome&ie=UTF-8&q=%22Prelude.head%3A+empty+list%22):

    Prelude.head: empty list

That's 6,610 results for this ridiculous exception. Alternatives are:

    listToMaybe :: [a] -> Maybe a
    take :: Int -> [a] -> [a]
    foldr :: (a -> b -> b) -> b -> [a] -> b
    data List a = Single a | Cons a (List a)
    data NonEmpty a = NonEmpty a [a]
    list :: (a -> [a] -> b) -> b -> [a] -> b
    Plain old pattern matching.

And if your code really does not make sense to have an empty list
(then why are you using a list data type?) and you cannot be swayed
against using a list type, please at least do this:

    throw MeaningfulException
       `fromMaybe` (listToMaybe xs)

Please stop using partial functions. Seriously. Remove them from your
codebase.

## Expanding with examples

Someone commented:

> What if my data type is a list and I know that head will not throw
> an exception?
>
>     foo [] = 0
>     foo xs = bar $ head xs

The problem is that this is an invariant that only exists in the
programmer's head (sorry) and is not encoded in the type system (such
is the problem with all partial functions), when it so easily can
be. Some examples:

Sometime last year I found a Haddock bug:

    haddock: internal Haddock or GHC error: Prelude.head: empty list

The cause is line 191:

    packageMod       = ifaceMod (head ifaces)

in the `render` function, because the author assumed that the
"not-null" invariant would never be broken. But then he used the
renderStep function again, and line 158, in the `main` function:

    renderStep flags packages []

Breaking this invariant.

Around this time I also found a bug in Hakyll due to use of tail:

    ./src/Text/Hakyll/Internal/CompressCss.hs:
    | otherwise = head str : stripComments (tail str)

which triggered [this exception](http://hpaste.org/40264/hakyll_error).

So, despite the invariant being satisfied at the time of writing,
later that tacit invariant was broken and the developer didn't realise
it. This is more or less the most common case of partial function
exceptions. You *just know* X will never happen, and then it
does.

It's trivial to abstract away partiality. In some cases handling cases
might be a speed concern, but that should be a case-by-case localized
optimization based on profiling.
