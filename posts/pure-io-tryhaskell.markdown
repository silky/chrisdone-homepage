---
date: 2014-01-12
title: Pure IO monad and Try Haskell
description: Pure IO monad and Try Haskell
author: Chris Done
tags: haskell
---

**tl;dr: [Try Haskell](http://tryhaskell.org/) now has simple I/O built
on a pure IO monad.**

I wrote Try Haskell in 2010. I didn't really update the codebase
since. It was rather poor, but had generally been stable enough. Last
week I finally set some time aside to rewrite it from scratch (barring
the tutorial part). In doing so I gave it a fresh design, rewrote the
backend and stopped using my patched version of mueval (faster, but
less stable).

Aside from reproducing the functionality, I afterwards added a new
piece of functionality: typing in a function name (or composition of
functions) will simply show the generalized type of that
expression. This removes the need for an explicit `:t` and is more
friendly to newbies who would rather see something useful than an
error.[^1]

After that, Bob Ippolito requested that some simple IO operations be
made available, like `getLine`, `putStrLn`, etc. People have requested
this in the past, but it seemed more complicated back then. This time,
it seemed rather easy to support simple input/output. The console
library already supports continued prompts, so the question became
simply how to do safe IO.

As Haskellers worth their salt know, the IO monad is not
special. Examples of a monad ADT for newbies are available[^2][^5],
and we know that pre-monad Haskell IO was modelled as
a request/response system[^3] before it got updated in Haskell 1.3[^4]
Fuelled by the nerdiness factor of this, and that it was the weekend,
I rose to the challenge. I knew it wouldn't be hard, but it would be
fun with a real use-case (Try Haskell's REPL).

My constraints were that it shouldn't be a continuation-based library,
because I cannot have any state in Try Haskell. The server evaluates
an expression and returns the result. No other context is kept, no
process kept open, and it should return immediately. Given that it's
rather hard to serialize closures, but rather easy to serialize a list
of inputs and outputs (aka responses/requests), I thought I'd go that
route.

In the end I settled on an `ErrorT` monad over a `State` monad
containing `Input` and an `Output`. The inputs would be
stdin lines as `[String]`. The outputs would be stdout lines and
either a final value, or an interrupt.

``` haskell
runIO :: Input -> IO a -> (Either Interrupt a, Output)
```

Whenever the expression being evaluated runs `getLine`, it reads from
the `Input` state and pops that line of text off the stack. When
`getLine` tries to read something and the stack is empty, it throws an
error (of the `ErrorT` monad), returning the interrupt
`InterruptStdin`. For example, here is a return value:

``` haskell
λ> runIO (Input mempty mempty) (return ())
(Right ()
,Output {outputStdout = [], outputFiles = fromList []})
```

Here is an interrupt:

``` haskell
λ> runIO (Input mempty mempty) (putStrLn "Name:" >> getLine)
(Left InterruptStdin
,Output {outputStdout = ["Name:\n"],outputFiles = fromList []})
```

As a user of the library it is now my part of the dance to get some
input from the user and then re-call[^6] the same function with more stdin
input:

``` haskell
λ> runIO (Input ["Chris"] mempty) (putStrLn "Name:" >> getLine)
(Right "Chris"
,Output {outputStdout = ["Name:\n"],outputFiles = fromList []})
```

I also implemented trivial exceptions:

``` haskell
λ> runIO (Input ["Chris"] mempty) (throw (UserError "Woo!"))
(Left (InterruptException (UserError "Woo!"))
,Output {outputStdout = [],outputFiles = fromList []})
```

After that, it was only a matter of time before I implemented some
simple file IO operations:

``` haskell
λ> runIO (Input ["Chris"]
                (M.fromList [("/foo.txt","Hello, World!")]))
         (readFile "/foo.txt")
(Right "Hello, World!"
,Output {outputStdout = []
        ,outputFiles = fromList [("/foo.txt","Hello, World!")]})
```

That resulted in the library
[pure-io](https://hackage.haskell.org/package/pure-io/docs/PureIO.html)
which
[some](http://ircbrowse.net/browse/haskell?id=17223900&timestamp=1389386757#t1389386757)
thought was a joke. It supports enough of the subset of IO operations
for, I think, a newbie to at least feel like they're doing some
realistic I/O. So I added it to [Try Haskell!](http://tryhaskell.org/) You can now run
interactive commands and write/save/list files. Any file system
operations you do will be saved in your browser's local storage.

It's really a rather nice referentially transparent IO service. Even
if you run `forever (getLine >>= putStrLn) :: IO ()`, it will run
forever, but the server can be restarted inbetween. No state is stored
on the server at all, it's all in the client. All the client has to do
is pass it back and forth when it communicates with the server.

I'd recommend Haskell intermediates (perhaps not newbies) to implement
their own IO monad as a free monad, or as an mtl transformer, partly
for the geeky fun of it, and partly for the insights.

[^1]: Like “No instance for `(Show (a0 -> a0))` arising from a use of …”
      which is frankly a useless message to print in a REPL and it's
      strange that this is GHCi's default behaviour.

[^2]: Johan Kiviniemi demonstrates
[an alternative monad implementation](https://gist.github.com/ion1/7154691)
as an ADT that you interpret.

[^3]:
[Request/response example API on StackOverflow](http://stackoverflow.com/questions/17002119/haskell-pre-monadic-i-o). All
shuffling between an actual interface and the user is left to someone
else to deal with.

[^4]: “Monadic I/O has already become the de-facto standard in the
various Haskell systems. We have chosen a fairly conservative, but
extensible basic design (an IO monad with error handling),” in
[the changes list](http://www.haskell.org/definition/from12to13.html#monad).

[^5]: Russell O'Connor also talks about implementing IO as a free
monad [here](http://r6.ca/blog/20110520T220201Z.html).

[^6]: Yes, that means running the same computation every time from
scratch, like a transaction.
