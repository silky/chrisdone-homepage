---
date: 2012-08-25
title: Making HaskellDB slightly more type-safe
description: Making HaskellDB slightly more type-safe
author: Chris Done
tags: haskell, haskelldb
---

I was just discussing HaskellDB's major flaws with Oliver Charles and
I noted that one *huge* problem is that the type of `update` does not
restrict the record given to make the update. Its type is

    update :: (ShowLabels s, ToPrimExprs s)
           => Database             -- ^ Database.
           -> Table r              -- ^ Entity.
           -> (Rel r -> Expr Bool) -- ^ Predicate.
           -> (Rel r -> Record s)  -- ^ Updates.
           -> IO ()

which is straight-forward enough. The problem is the “updates”
argument, which will allow me to write a bogus field that does not
belong to `r`, like

    update mytable
           (\row -> row!field .==. whatever)
           (\row -> badfield <<- whatever)

This problem actually bit me in the ass in production once
before. That is not an exciting bug to have.

So I thought, we need to prove that for the type above, `s <: r` (read
as “s is a
[subtype](http://en.wikipedia.org/wiki/Subtype_polymorphism) of
r”). How do we express that? How about a type class.

The type-class can be

    class Subset sub super

But how to implement it? Well, we need to say that for every `field`
of sub, that `field` is also a field of `super`. That's made easy for
us, because HaskellDB already has a `HasField field record` class for
exactly that!

    instance (HasField field super,Subset sub super) =>
             Subset (RecCons field typ sub) super

This is similar to traversing a list at the value level, with `RecCons
field type sub` like a pattern-match on the current element. You can
read it as:

> `sub` is a subset of `super`, if `super` has the `field` of the
>  head of the list, and the tail is a subset of super

So far so good. Now we need a base case, to cover the last element of
the list:

    instance Subset RecNil super

And we're done. Update now becomes

    update :: (Subset s r,ShowLabels s, ToPrimExprs s)
           => Database             -- ^ Database.
           -> Table r              -- ^ Entity.
           -> (Rel r -> Expr Bool) -- ^ Predicate.
           -> (Rel r -> Record s)  -- ^ Updates.
           -> IO ()

Testing this on my codebase actually found a bug in which I was using
the wrong field!

I will send this to the maintainer of HaskellDB as it's a glaring bug
waiting to happen to someone.
