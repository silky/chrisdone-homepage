---
date: 2014-04-22
title: Typeable and Data in Haskell
description: Typeable and Data in Haskell
author: Chris Done
tags: haskell
---

[Data.Typeable](hackage.haskell.org/package/base-4.6.0.1/docs/Data-Typeable.html)
and
[Data.Data](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Data.html)
are rather mysterious. Starting out as a Haskell newbie you see them
once in a while and wonder what use they are. Their Haddock pages are
pretty opaque and scary in places. Here's a quick rundown I thought
I'd write to get people up to speed nice and quick so that they can
start using it.[^1]

It's really rather beautiful as a way to do generic programming in
Haskell. The general approach is that you don't know what data types
are being given to you, but you want to work upon them almost as if
you did. The technique is simple when broken down.

## Requirements

First, there is a class exported by each module. The class `Typeable`
and the class `Data`. Your data types have to be instances of these if
you want to use the generic programming methods on them.

Happily, we don't have to write these instances ourselves (and in GHC
7.8 it is actually not possible to do so): GHC provides the extension
`DeriveDataTypeable`, which you can enable by adding `{-# LANGUAGE
DeriveDataTypeable #-}` to the top of your file, or providing
`-XDeriveDataTypeable` to `ghc`.

Now you can derive instances of both:

``` haskell
data X = X
  deriving (Data,Typeable)
```

Now we can start doing generic operations upon `X`.

## The Typeable class

As a simple starter, we can trivially print the type of any instance
of `Typeable`. What are some existing instances of `Typeable`? Let's
ask GHCi:

``` haskell
λ> :i Typeable
class Typeable a where typeOf :: a -> TypeRep
instance [overlap ok] (Typeable1 s, Typeable a) => Typeable (s a)
instance [overlap ok] Typeable TypeRep
instance [overlap ok] Typeable TyCon
instance [overlap ok] Typeable Ordering
instance [overlap ok] Typeable Integer
instance [overlap ok] Typeable Int
instance [overlap ok] Typeable Float
instance [overlap ok] Typeable Double
instance [overlap ok] Typeable Char
instance [overlap ok] Typeable Bool
instance [overlap ok] Typeable ()
```

That's the basic Prelude types and the Typeable library's own
types.

There's only one method in the `Typeable` class:

``` haskell
typeOf :: a -> TypeRep
```

The `TypeRep` value has some useful normal instances:

``` haskell
λ> :i TypeRep
instance [overlap ok] Eq TypeRep
instance [overlap ok] Ord TypeRep
instance [overlap ok] Show TypeRep
instance [overlap ok] Typeable TypeRep
```

## Use-case 1: Print the type of something

So we can use this function on a `Char` value, for example, and GHCi
can print it:

``` haskell
λ> :t typeOf 'a'
typeOf 'a' :: TypeRep
λ> typeOf 'a'
Char
```

This is mostly useful for debugging, but can also be useful when
writing generic encoders or any tool which needs an identifier to be
associated with some generic value.

## Use-case 2: Compare the types of two things

We can also compare two type representations:

``` haskell
λ> typeOf 'a' == typeOf 'b'
True
λ> typeOf 'a' == typeOf ()
False
```

Any code which needs to allow any old type to be passed into it, but
which has some interest in sometimes enforcing or triggering on a
specific type can use this to compare them.

## Use-case 3: Reifying from generic to concrete

A common thing to need to do is when given a generic value, is to
sometimes, if the type is right, actually work with the value as the
concrete type, not a polymorphic type. For example, a printing
function:

``` haskell
char :: Typeable a => a -> String
```

The specification for this function is: if given an `Char`, return its
string representation, otherwise, return `"unknown"`. To do this, we
need a function that will convert from a polymorphic value to a
concrete one:

``` haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

This function from `Data.Typeable` will do just that. Now we can
implement `char`:

``` haskell
λ> let char x = case cast x of
                  Just (x :: Char) -> show x
                  Nothing -> "unknown"
λ> char 'a'
"'a'"
λ> char 5
"unknown"
λ> char ()
"unknown"
```

## The Data class

That's more or less where the interesting practical applications of
the `Typeable` class ends. But it becomes more interesting once you
have that, the `Data` class can take advantage of it. The `Data` class
is much more interesting. The point is to be able to look into a data
type's constructors, its fields and traverse across or fold over
them. Let's take a look at the class.

Again, there are some basic instances provided:

``` haskell
instance Data a => Data [a]
instance Data Ordering
instance Data a => Data (Maybe a)
instance Data Integer
instance Data Int
instance Data Float
instance (Data a, Data b) => Data (Either a b)
instance Data Double
instance Data Char
instance Data Bool
```

It's a rather big class, so I'll just cover some methods that
demonstrate the key use-cases.

## Use-case 1: Get the data type

Similar to the `TypeRep`, you can use `dataTypeOf` to get a
unique representation of a data type:

``` haskell
dataTypeOf :: Data a => a -> DataType
```

For example:

``` haskell
λ> dataTypeOf (Just 'a')
DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}
```

There aren't any other interesting instances for this type, but we'll
look at uses for this type later. Representations (so-called `FooRep`)
tend to be references from which you can reify into more concrete
values.

## Use-case 2: Inspecting a data type

The most common thing to want to do is to get a list of constructors
that a type contains. So, the `Maybe` type contains two.

``` haskell
λ> :t dataTypeConstrs
dataTypeConstrs :: DataType -> [Constr]
λ> dataTypeConstrs (dataTypeOf (Nothing :: Maybe ()))
[Nothing,Just]
```

We'll look at what we can do with constructors later.

It's also surprisingly common to want to see what the constructor is
at a particular index. We could write this function ourself, but
there's already one provided:

``` haskell
λ> indexConstr (dataTypeOf (Nothing :: Maybe ())) 2
Just
```

Sometimes you want to know whether a data type is algebraic (in other
words, does it have constructors and is it not one of the built-in
types like Int/Float/etc)?

``` haskell
λ> isAlgType (dataTypeOf (Just 'a'))
True
λ> isAlgType (dataTypeOf 'a')
False
```

![](http://i.imgur.com/7ud0ezn.gif)

## Use-case 3: Get the constructor of a value

We have the method

``` haskell
toConstr :: a -> Constr
```

Which given any instance of `Data` will yield a constructor.

``` haskell
λ> :i Constr
data Constr
instance Eq Constr
instance Show Constr
```

You can't do much with a constructor as-is, but compare and print it:

``` haskell
λ> toConstr (Just 'a')
Just
λ> toConstr (Just 'a') == toConstr (Nothing :: Maybe Char)
False
```

However, those operations by themselves can be useful.

By the way, we can also get back the `DataRep` of a constructor:

``` haskell
λ> constrType (toConstr (Just 'a'))
DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}
```

## Use-case 4: Get fields of a constructor

Another typical thing to want to do is to use the field names of a
constructor. So for example:

``` haskell
λ> data X = X { foo :: Int, bar :: Char } deriving (Typeable,Data)
λ> toConstr (X 0 'a')
X
λ> constrFields (toConstr (X 0 'a'))
["foo","bar"]
```

It's a good use-case for serializing and debugging.

## Use-case 5: Make a real value from its constructor

It's actually possible to produce a value from its constructor. We
have this function

``` haskell
fromConstr :: Data a => Constr -> a
```

Example:

``` haskell
λ> fromConstr (toConstr (Nothing :: Maybe ())) :: Maybe ()
Nothing
```

But what do you do when the constructor has fields? No sweat. We have
this function:

``` haskell
fromConstrB :: forall a. Data a
            => (forall d. Data d => d) -> Constr -> a
```

![](http://i.imgur.com/vs9AG.gif)

Haskell beginners: Don't fear the rank-N type. What it's saying is
merely that the `fromConstrB` function determines what the type of `d`
will be by itself, by looking at `Constr`. It's not provided
externally by the caller, as it would be if the `forall d.` were at
the same level as the `a`. Think of it like scope. `let a = d in let d
= …` doesn't make sense: the `d` is in a lower scope. That means we
can't just write:

``` haskell
fromConstrB (5 :: Int) (toConstr (Just 1 :: Maybe Int)) :: Maybe Int
```

The `Int` cannot unify with the `d` because the quantification is one
level lower. It basically doesn't exist outside of the `(forall
d. Data d => d)` (nor can it escape). That's okay, though. There is a
type-class constraint which lets us be generic. We already have a
function producing a value of that type:

``` haskell
λ> :t fromConstr (toConstr (1 :: Int))
fromConstr (toConstr (1 :: Int)) :: Data a => a
```

So we can just use that:

``` haskell
λ> fromConstrB (fromConstr (toConstr (1 :: Int)))
               (toConstr (Just 1 :: Maybe Int)) :: Maybe Int
Just 5
```

Tada! But wait? What if there're _more_ fields. How do we provide more
than one, and of different types?

Enter `fromConstrM`:

``` haskell
fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d) -> Constr -> m a
```

Because it's monadic we can use a state monad to keep an index!
Observe:

``` haskell
λ> :t execState
execState :: State s a -> s -> s
λ> :t execState (modify (+1))
execState (modify (+1)) :: Num s => s -> s
λ> :t execState (forM_ [1..5] (const (modify (+1))))
execState (forM_ [1..5] (const (modify (+1)))) :: Num s => s-> s
λ> execState (forM_ [1..5] (const (modify (+1)))) 5
10
```

Let's put this to use with `fromConstrM`:

``` haskell
λ> evalState
     (fromConstrM
       (do i <- get
           modify (+1)
           return
             (case i of
               0 -> fromConstr (toConstr (5::Int))
               1 -> fromConstr (toConstr 'b')))
       (toConstr (Foo 4 'a')))
     0 :: Foo
Foo 5 'b'
λ>
```

In other words, keep an index starting at 0. Increase it each
iteration that `fromConstrM` does. When we're at index 0, return an
`Int`, when we're at index 1, return a `Char`. Easy!

## Use-case 6: mapping over data structures generically

A common thing to want is to map over a value in a
structure-preserving way, but changing its values. For that we have
`gmapT`:

``` haskell
gmapT :: forall a. Data a
      => (forall b. Data b => b -> b) -> a -> a
```

Similar to `fromConstr*`, there is a rank-n type `b` that refers to
each type in the constructor of type `a`. It's easy enough to use:

``` haskell
λ> gmapT
     (\d ->
        case cast d of
          Nothing -> d
          Just x ->
            fromJust (cast (if isUpper x then '!' else x)))
     (Foo 4 'a')
Foo 4 'a'
λ> gmapT
     (\d ->
        case cast d of
          Nothing -> d
          Just x ->
            fromJust (cast (if isUpper x then '!' else x)))
     (Foo 4 'A')
Foo 4 '!'
```

Here I'm doing a little check on any field in the constructor of type
`Char` and if it's upper case, replacing it with `!`, otherwise
leaving it as-is. The first trick is to use the `cast` function we
used earlier to reify the generic `d` into something real
(`Char`). The second trick is to cast our concrete `Char` back into a
generic `d` type.

Just like `fromConstrM` earlier, if you want to operate on exact
indices of the constructor rather than going by type, you can use
`gmapM` and use a state monad to do the same thing as we did before.

## Use-case 7: generating from data structures generically

Another slightly different use-case is to walk over the values of a
data structure, collecting the result. You can do this with `gmapM`
and a state monad or a writer, but there's a handy function already to
do this:

``` haskell
gmapQ :: forall a. Data a => (forall d. Data d => d -> u) -> a -> [u]
```

Trivial example:

``` haskell
λ> gmapQ (\d -> toConstr d) (Foo 5 'a')
[5,'a']
```

A more useful example can be found in
[structured-haskell-mode](https://github.com/chrisdone/structured-haskell-mode/blob/18c011978acfca30bac800d0ac0e9e31e653c440/src/Main.hs#L96)
which walks over the Haskell syntax tree and collects source spans
into a flat list.

## Printer example

Here's a trivial (not very good, but something I wrote once) generic
printer:

``` haskell
gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple = showChar '('
              . drop 1
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))
          isList = constructor "" == "(:)"
```

I wrote it because the GHC API doesn't have `Show` instances for most
of its data types, so it's rather hard to actually inspect _any_ data
types that you're working with in the REPL. It has instances for
pretty printing, but pretty printing confuses presentation with data.

Example:

``` haskell
λ> data Foo = Foo Char Int deriving (Data,Typeable)
λ> gshow ([Just 2],'c',Foo 'a' 5)
"([(Just (2))],('c'),(Foo ('a') (5)))"
```

Note: no `Show` instance for `Foo`.

## Summary

We've briefly covered how to query types, how to cast them, how to
walk over them or generate from them. There're other things one can
do, but those are the main things. The real trick is understanding how
to make the types work and that comes with a bit of experience. Fiddle
around with the concepts above and you should gain an intution for
what is possible with this library. See also:
[Data.Generics.Aliases](http://hackage.haskell.org/package/syb-0.4.1/docs/Data-Generics-Aliases.html).

Hope it helps!


[^1]: I'll migrate this to the HaskellWiki when it doesn't look so, uh,
shall we say, unattractive.
