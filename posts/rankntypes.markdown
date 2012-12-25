---
date: 2011-10-16
title: Rank-N types, a simple DB example
description: Rank-N types, a simple DB example
author: Chris Done
tags: haskell, types
---

This is a very simple example of rank-N types to demonstrate to
non-Haskellers/newbies.

Following the resources theme, rank-N types as seen in
[the ST monad](http://www.haskell.org/haskellwiki/Monad/ST) are also a
gem:

    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE RankNTypes #-}

Define a monad for interacting with the DB parametrized on a
connection type variable, `c`, as well as the return type:

    newtype DB c a = DB (IO a) deriving (Monad)

And a connection data type parametrized on the connection type variable:

    newtype Connection c = Connection ()

Define the connection opening function such that the quantified type
variable `c` cannot escape from the DB monad:

    withConnection :: (forall c. DB c a) -> IO a
    withConnection m = case m of DB io -> io

Let's say in a real implementation `withConnection` opens a database
connection in a transaction and commits and closes when done, among
other exception catching things.

Define some functions for the DB monad (note they all reference the
`c` type variable):

    getConn :: DB s (Connection c)
    getConn = return (Connection ())

    query :: Connection c -> String -> DB c [String]
    query _ _ = return ["Hello!"]

Now we can use it like this:

    demo1 = withConnection $ do
      conn <- getConn
      rows <- query conn "SELECT …"
      return rows

    λ> demo1
    ["Hello!"]

But if you try to return the connection…

     demo2 = withConnection $ do
       conn <- getConn
       rows <- query conn "SELECT …"
       return conn

You get a compile error:

    Error:  Inferred type is less polymorphic than expected
          Quantified type variable `c' escapes

This is pretty nice if your DB library implementation, e.g., is
supposed to ensure operations on a connection run inside a
transaction, or if your operations assume a connection
exists. Otherwise you're liable to having DB code run outside of a
transaction, or code throwing exceptions because the connection was
closed but we tried to use it anyway, or in severe cases, some C DB
libraries will just segfault.

We didn't have to do anything complex or write any boilerplate or
macros or whatnot, just use the type system. That's what it's for.
