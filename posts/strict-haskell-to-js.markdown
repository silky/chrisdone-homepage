---
date: 2014-05-02
title: Strict Haskell to JS?
description: Strict Haskell to JS?
author: Chris Done
tags: haskell, javascript
---

This is just an idea post. It occurred to me recently that you can do
two things:

1. Make a strictness enforcer as a package for Haskell. You could call
   it haskell-strict. It would check that a given module is strict.
2. Once you've run your module through haskell-strict, you can now
   compile it to JavaScript strictly with no runtime.

Let's explore the idea!

## haskell-strict

What would such a package do? There is a strict subset of (GHC)
Haskell. It's like normal Haskell, but with the following constraints:

1. All function arguments must be banged.
2. Lazy patterns are disallowed.
3. Let/where/top-level patterns are disallowed.
4. All top-level variables must be functions, or non-self-referencing.
5. All data structures must have strict fields.

The latter three pretty much follow from the second rule. What does
this result in?

``` haskell
fib !0 = 0
fib !1 = 1
fib !n = fib (n - 1) + fib (n - 2)

foo !x =
  let !bar = bob
  in case qux of
       !(!x,!y) -> cam
  where !mu = zot
```

Ugly, but not too ugly -- tuples suffer the most. But data
user-defined structures don't suffer at all:

``` haskell
data Foo = Foo !Int !Char
data Person = Person { name :: !Text, age :: !Int }

foo = case Person "hi" 123 of
        Person name age -> bar
```

Note that cases are strict by default (compare: `~(Person name age)`
which would make this case lazy), and the strict fields will be forced
by this case by virtue of the bangs.

What's illegal?

``` haskell
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
(x,y) = (y,x)
```

There're some subtleties, but that's the gist of it.

## Compiling to JavaScript

Now that our module has been stripped down to a strict core, there are two things to
consider:

1. No need for thunks anymore, or a runtime to accompany it.
2. Infinite recursion will require tail-recursive optimization.

The first is cool. Let's explore it:

``` haskell
sum acc 0 = acc
sum acc n = sum (acc + n) (n - 1)
```

This function is not strict Haskell yet. The `haskell-strict` tool
would complain that it is lacking bang patterns. So we fix that:

``` haskell
sum !acc !0 = acc
sum !acc !n = sum (acc + n) (n - 1)
```

Now the compiled JavaScript, what might that look like? Perhaps:

``` javascript
function sum(acc){
  return function(n){
    if (n == 0) {
      return acc;
    } else {
      return sum(acc + n)(n - 1);
    }
  }
}
```

Not too shabby. In fact, it's exactly the same code with different
syntax. Of course, with trivial saturation detection, we can collapse
this:

``` javascript
function sum(acc,n){
  if (n == 0) {
    return acc;
  } else {
    return sum(acc + n,n - 1);
  }
}
```

Now, we note that this will exhaust the stack. Such code is perfectly
fine JavaScript code, but it's not efficient. We don't have while
loops in Haskell, so recursion is what we use. We have to optimize the
tail call here. Let's do that:

``` javascript
function sum(acc,n){
  var _acc = acc, _n = n;
  while (true) {
    n = _n, acc = _acc;
    if (n == 0) {
      return acc;
    } else {
      _acc = acc + n, _n = n - 1;
    }
  }
}
```

Pretty trivial transformation. 100% efficient. Clean.

Data structures are also easily dealt with:

``` haskell
data Person = Person { name :: !Text, age :: !Int }
foo !x = name x
```

Produces:

``` javascript
var Person = function(name,age){ this.name = name, this.age = age; };
function foo(x){ return x.name };
```

Note that this style of object declaration in JavaScript is a
[lambdillion times faster than object literals](http://jsperf.com/object-create-vs-constructor-vs-object-literal/7).

## Why?

Good question. I'm not sure. But it's a possibility. PureScript serves
a similar niche. Roy also compiles to very clear 1-to-1 JavaScript. If
you want to write Haskell code that will produce nearly exactly the
same, performant, JavaScript, and you have an immoral amount of time on
your hands, this might be an avenue for you to explore.
