---
date: 2014-03-30
title: One step forward, two steps back
description: One step forward, two steps back
author: Chris Done
tags: programming, haskell
---

The issue is that programming languages don't go forward, they move
sideways or diagonally, or sometimes backwards.

A new car comes out, and it has some cool feature: Hey, it has road
surface detection and changes the steering accordingly! But it should
also come with all the old stuff that you come to expect. Comfy seats,
seatbelts, airconditioner, heated windows, wipers, proximity
detection, power steering, cruise control, etc.

With new programming languages, what you tend to get is a chassis,
engine and steering wheel, and the road surface detection.

Here is a list of cool ideas that have been discovered and implemented
in programming languages, but which do not in their whole make up any
existing language:

* [Contracts](http://en.wikipedia.org/wiki/Design_by_contract)
* [Pattern matching](http://en.wikipedia.org/wiki/Pattern_matching)
* [Syntactic abstraction](http://c2.com/cgi/wiki?SyntacticAbstraction)[^1]
* [Static types](http://en.wikipedia.org/wiki/Type_system)
* [Type inference](http://en.wikipedia.org/wiki/Type_inference)
* [Dependent types](http://en.wikipedia.org/wiki/Dependent_type)
* [Parametrized modules](http://en.wikipedia.org/wiki/Standard_ML#Module_system)
* [Overloaded literals](http://www.haskell.org/ghc/docs/7.0.4/html/users_guide/type-class-extensions.html#overloaded-strings)[^5]
* [Fundamental immutability](http://en.wikipedia.org/wiki/Immutable_object)
* [Purity](http://en.wikipedia.org/wiki/Purely_functional)
* [Interpreter](http://en.wikipedia.org/wiki/Interpreter_%28computing%29)
* [Closures](http://en.wikipedia.org/wiki/Closure_%28computer_programming%29)
* [Multiple dispatch](http://en.wikipedia.org/wiki/Multiple_dispatch)
* [Inheritance](http://en.wikipedia.org/wiki/Inheritance_%28object-oriented_programming%29)
* [Garbage collection](http://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29)
* [A debugger](http://en.wikipedia.org/wiki/Debugger)
* [Well-supported concurrency](http://en.wikipedia.org/wiki/Concurrency_%28computer_science%29)
* [Parallelism](http://en.wikipedia.org/wiki/Parallel_computing)
* [Dynamic scope](http://en.wikipedia.org/wiki/Scope_%28computer_science%29#Dynamic_scoping)[^3]
* [Dynamic code update](http://en.wikipedia.org/wiki/Hot_swapping)[^2]
* [Image-based persistent](http://en.wikipedia.org/wiki/Smalltalk#Image-based_persistence)
* Infinite-sized integers and rationals
* [Exceptions](http://en.wikipedia.org/wiki/Exception_handling)
* [Explicit null](http://en.wikipedia.org/wiki/Nullable_type)
* [Generic equality](http://www.haskell.org/tutorial/classes.html)
* [Restarts](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html)
* A comprehensive platform / library set (e.g. Java, .NET)
* [REPL/interaction mode](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
* [Compiler—IDE integration](http://common-lisp.net/project/slime/)
* Interoperability (with C, Java, JavaScript, CLR)
* [Support domain-specific languages](http://en.wikipedia.org/wiki/Domain-specific_language)
* [Proofs](http://en.wikipedia.org/wiki/Proof_assistant)
* [Distribution](http://en.wikipedia.org/wiki/Erlang_%28programming_language%29#Concurrency_and_distribution_orientation)
* A compiler that compiles to machine code, either via static
  compilation, JIT compilation, or third-party engines
* [Tacit programming](http://en.wikipedia.org/wiki/Tacit_programming)
* [Value polymorphism](http://chrisdone.com/posts/value-polymorphism)

The moral is, perhaps if you're making a new language, try building
upon the past, rather than ignoring it and starting from scratch?

As a follow-up post I might make a matrix of the top, say, 30, general
purpose programming languages and all the features that they tick off.

[^1]: Also known as quotation, quasiquotes, macros, templating,
      mixins, etc.
[^2]: Also known as "hot-swapping", "live update", "plugins", etc.

[^3]: Preferablly static. Also known as implicit parameters, contexts, etc.

[^5]: So, numbers, strings, vectors, patterns, etc.
