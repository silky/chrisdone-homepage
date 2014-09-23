---
date: 2014-04-01
title: Haskell structured diffs
description: Haskell structured diffs
author: Chris Done
tags: haskell, diff
---

Project-request: someone please implement a program that will diff
Haskell in a cleverer way than lines.

In an effort to reign in my incessant work on Haskell tooling[^1], I'm
outlining a tool that I'd personally like and welcome people to
implement it. Otherwise it serves as a motivating problem description
for the next time I come around to it myself with free time.

Before anyone emails me saying “lines/words are simple, other things
are hard, that's why it's not been done yet. People undervalue the
simple solution …” with a long lecture, spare me!

## The concrete diff

The concrete diff is the line-based, and sometimes character-based,
diff that we all know and love. There's no reason to throw this
away. You will need to keep this as an optional backend for when you
are unable to parse a Haskell file.

Pros: simple to implement. You produce the necessary lines to delete
and insert to create the change from A to B.

Cons: doesn't know about syntactic redundancy where some changes don't
mean anything, and where the actual important change occurs. For
example:

``` haskell
main = do putStrLn "Write your name!"
          name <- getLine
          print name
```

Now you change this to:

``` haskell
main = do args <- getArgs
          case args of
            [] -> do putStrLn "Write your name!"
                     name <- getLine
                     print name
            _ -> runWithArgs args
```

The diff will look like this:

``` diff
@@ -5,3 +5,6 @@ module Main where
-main = do putStrLn "Write your name!"
-          name <- getLine
-          print name
+main = do args <- getArgs
+          case args of
+            [] -> do putStrLn "Write your name!"
+                     name <- getLine
+                     print name
+            _ -> runWithArgs args
```

But it's clear to observe that this is not the change we made in
spirit, it's just one line-based way to achieve it. In actual fact,
our `do putStrLn …` was moved into a `case`, un-changed. At this size,
it's not a big deal. When the code is more interesting, it's important
to know what was really changed, and what remains the same.

## The abstract syntax diff

Enter the syntactic diff. We show the difference between two syntactic
trees. How this is to be achieved in a readable way is the rub, but
here are some ideas.

Take our example above, one approach can be to label nodes.

Before:

``` haskell
¹{main = ²{do putStrLn "Write your name!"
              name <- getLine
              print name}}
```

After:

``` haskell
¹{main = do args <- getArgs
            case args of
              [] -> ²{do putStrLn "Write your name!"
                         name <- getLine
                         print name}
              _ -> runWithArgs args}
```

Now, at least at a superficial glance, you don't even need this
explained to you. You can see exactly what has happened: The code
before has changed to the code after, but we can see that
node<sub>2</sub> has just moved to inside the case.

Where the trickiness arises is taking this to its logical conclusion
and applying it generally. What's displayed if you also change the
string in the `putStrLn`? Good question. Here's an idea:


``` haskell
¹{main = ²{do putStrLn -{"Write your name!"}
              name <- getLine
              print name}}
```

Because the node `"Write your name"` has now been lost, we don't need
to reference it any longer. So one way to show that it has been
removed could be to put `-{…}`. And then to show what replaced it, put
in `+{…}`, a la classic diffs:

``` haskell
¹{main = +{do args <- getArgs
              case args of
                [] -> ²{do putStrLn +{"Hello!"}
                           name <- getLine
                           print name}
                _ -> runWithArgs args}}
```

In reality this rule would insert more `-{…}` and `+{…}` than I've
written here, but I'm writing these examples manually so take them
with a grain of salt. Let's take it further and say that the string
has actually been moved. Then we should indeed give it a number to
reference it later:

Before:

``` haskell
¹{main = ²{do putStrLn ³{"Write your name!"}
              name <- getLine
              print name}}
```

After:

``` haskell
¹{main = +{do args <- getArgs
              case args of
                [] -> ²{do putStrLn +{greeting}
                           name <- getLine
                           print name}
                _ -> runWithArgs args}
    +{where greeting = ³{"Write your name!"}}}
```

Again, I don't think anybody is going to find this confusing. The
node<sub>3</sub> has moved into a `where` clause, which has been named
`greeting` and referenced in place of its original place.

Am I making obvious sense, here? It's not a particularly novel
display, it states what happened syntactically, precisely. With a UI,
you could expand/collapse nodes in a nested fashion or "explode" all
the pieces into a flat list of numbered or +'d or -'d nodes, or just
narrow down to one specific interesting expression, like

``` haskell
²{do putStrLn +{greeting}
     name <- getLine
     print name}
```

If you're sufficiently nerd-sniped to find this interesting and
do-able, then I invite you to go ahead and give it a go. I'd love
to see a prototype. I don't plan on implementing this in the near or
distant future, so we won't be toe stepping.

## The reduced semantic diff

If you're still reading by this point, let me try to entice you with
ambitious ideas. Take the above approach, everything we just laid out,
but let's put an additional step in there: instead of diffing
Haskell's abstract syntax tree, diff the Core.

If you compile the below with GHC,

``` haskell
main = case Just () of
         Just () -> print "Hey!"
```

The external core is:

``` haskell
%module main:Main
  main:main5 :: (ZMZN Char) = unpackCStringzh ("Hey!"::Addrzh);
  main:main4 :: (ZMZN Char) = ZC @ Char base:zdfShowChar1 (ZMZN @ Char);
  main:main3 :: (ZMZN Char) = base:showLitString main:main5 main:main4;
  main:main2 :: (ZMZN Char) = ZC @ Char base:zdfShowChar1 main:main3;
  main:main1 :: (Statezh RealWorld) -> (Z2H ((Statezh RealWorld)) Z0T) =
    \ (etaB1::(Statezh RealWorld)) ->
      base:hPutStr2 base:stdout main:main2 True etaB1;
  main:main :: (IO Z0T) = %cast (main:main1) (%sym ((NTCoZCIO Z0T)));
  main:main6 :: (Statezh RealWorld) -> (Z2H ((Statezh RealWorld)) Z0T) =
    \ (etaXb::(Statezh RealWorld)) ->
      base:runMainIO1 @ Z0T (%cast (main:main1) (%sym ((NTCoZCIO Z0T))))
                       etaXb;
  main:ZCmain :: (IO Z0T) = %cast (main:main6) (%sym ((NTCoZCIO Z0T)));
```

You can see that the pointless `case` has been removed. This is the
bread and butter of Core simplification. But if I remove the case
myself, the Core is exactly the same. This is redundant semantic
content, which is why GHC removed it.

If someone made a change like this in a real codebase which removed
some redundant _semantic_ content, not just syntactical redundancy,
your diff could show it like that. In other words, nothing important
semantically actually happened here.

In fact, if I refactored a bunch of code, re-organized a bit, does my
next colleague really want to read through all the syntax tree just to
see the crux of what changed? Sometimes, but not always. Sometimes,
they just want to see the precise thing that will change at runtime.

It might actually be insane, with big blow ups in code difference for
minute high-level changes, or it might be great for teams caring about
performance. Difficult to know until you try it. You can also do a
source-mapping back to the original Haskell source, for a more
interesting display.

If you want to implement this, I would love to see any results.

## The typed diff

Okay, you're still reading so you're pretty easily nerd sniped. Let's
continue with the ideas. Another type of difference between two
sources is the types of expressions in there. Consider:

``` haskell
main = let x = [1,2,3]
       in print (x <> x)
```

Now you change the code to:

``` haskell
main = let x = myFancyMonoid
       in print (x <> x)
```

Our structural diff laid out earlier will show this:

``` haskell
main = let x = -{[1,2,3]}
       in print (x <> x)
```

After:

``` haskell
main = let x = +{myFancyMonoid}
       in print (x <> x)
```

But actually, more things have changed here. As a result of the
different monoid instance, the `print (x <> x)` will do something
different. Maybe it's a `*` rather than `+`, maybe it's a number,
whatever. Maybe that expression is used in a more interesting way than
merely printing it. What's the real diff?

``` haskell
main = let {x::[Integer]} = -{[1,2,3]}
       in print {{(x <> x)}::[Integer]}
```

After:

``` haskell
main = let {x::MyFancyMonoid} = +{myFancyMonoid}
       in print {(x <> x)}::MyFancyMonoid}
```

Or something like that. I'm being hand-wavey in the display, here. The
real difference is that we've changed the type of `x`. It's an
important change, which has semantic meaning. My ideas are more vague
here. I haven't thought through many scenarios of how to represent
this. But one thing is clear: a diff of types can actually be useful
and interesting.

## The editing diff

The diffs above are all examples of "cold" diffs. Calculating the
difference between two files as-is. If you're in a structured editor
like [Lamdu](http://peaker.github.io/lamdu/), then you don't have to
do cold diffs and figure out and guess at what happened. You know
exactly what happened. This node was raised here, this variable was
renamed there, etc. But if you want to work on that, you pretty much
have to work on Lamdu.

## Summary

In summary I've intentionally listed increasingly more wacky diff
ideas, from the familiar to the fairly novel. My general approach to
tooling is progressive: start with the simplest working implementation
then step up. Structured-haskell-mode is an example of this. It's no
Lamdu, and it's no vanilla text-based mode. It's a stepping stone
inbetween. The impedance to try SHM is lower.

In the same way, maybe we can start with the abstract syntax diff, let
people become acclimatized to it, let it stabilize, get it integrated
into things like Git, and then work our way up from there.

If nobody bothers trying out these ideas, I'll probably end up doing
them myself eventually, but I thought I'd put the suggestion out there
first.

[^1]: In favour of writing programs that concern themselves with
      things other than Haskell for once!
