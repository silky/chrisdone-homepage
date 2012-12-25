---
date: 2010-11-22
title: Duck typing in Haskell
description: Duck typing in Haskell
author: Chris Done
tags: haskell
---

This is a simple Literate Haskell file demonstrating
[duck typing](http://en.wikipedia.org/wiki/Duck_typing) in
Haskell. You copy the whole web page and paste it into a .hs file and
compile/load it. Grab the normal Haskell version
[here.](http://hpaste.org/41710/duck_typing)

I'm using [a library called Has which you can grab from
Hackage](http://hackage.haskell.org/package/has) (or just `cabal
install has`). It's pretty neat,
it allows you to define field names and their types, that you are
going to use at some point, and then lets you construct/access/update
arbitrary records based on those fields.

We need to enable type families and flexible class contexts to work
with this library. And also it's nice if we disable the monomorphism
restriction. I don't want mono, I want manymorphism! As many as you
can fit in your pocket.

    > {-# LANGUAGE TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}


In this library you define fields like this:

    data Flap = Flap; type instance TypeOf Flap = IO ()

Here I'm defining a "flap" method. Ducks flap! The `Flap` part is the
accessor. But it sucks to have to write that, so we define a
synonym. So the whole field definition ends up being:

    data Flap = Flap; type instance TypeOf Flap = IO (); flap = Flap

Which is getting a little tedius, so I whipped up a little template
haskell to make this easier. You can install that
[from Hackage too](http://hackage.haskell.org/package/has-th) (or just
`cabal install has-th`). So let's enable template haskell:

    > {-# LANGUAGE TemplateHaskell #-}


Now I just import the Has library and my template haskell module.

    > import Data.Has hiding (field)
    > import Data.Has.TH


And defining accessors is really easy:

    > field "flap"  [t|IO ()|]
    > field "quack" [t|String|]
    > field "swim"  [t|IO ()|]
    > data Food = Bread | Pasta | Duck deriving Show
    > field "eat"   [t|Food -> Either String String|]
    > field "name"  [t|String|]
    > field "age"   [t|Integer|]


The `[t|Foo|]` is just a reader macro for reading types that exist.

So I've defined a bunch of things that a duck generally *does* and
*has*. Referring to James Whitcomb Riley:

> "When I see a bird that walks like a duck and swims like a duck and quacks like a duck, I call that bird a duck."

Now let's make some ducks! The obvious being Donald Duck:

    > donald = flap  ^- putStrLn "*Flap flap flap*"
    >        & quack ^- "QUACK!"
    >        & swim  ^- putStrLn "*Ducky paddle*"
    >        & eat   ^- nom
    >        & name  ^- "Donald Duck"
    >   where nom Bread = Right "Nom nom nom!"
    >         nom Pasta = Right "Ew, give me something bweady."
    >         nom Duck  = Left  "Are you cwazy!?"


He flaps, quacks, swims, eats and has a name. Also, I'll pretend to be
a duck, too:

    > chris = flap  ^- putStrLn "I'm flapping my arms!"
    >       & quack ^- "Erm, quack?"
    >       & swim  ^- putStrLn "I'm doing the butterfly because it's efficient ..."
    >       & age   ^- 67
    >       & eat   ^- nom
    >       & name  ^- "Chris Done"
    >   where nom Bread = Left  "Bread is alright, but no thanks."
    >         nom Pasta = Right "Pasta is okay. Got anything to go with it?"
    >         nom Duck  = Right "Om nom nom."


Notice that I've got one more field than `donald`, I've got an `age`. This means we are two distinct record types. We've got different stuff! But we're both ducks! See:

    λ> flap ^. donald
    *Flap flap flap*

    λ> flap ^. chris
    I'm flapping my arms!

    λ> quack ^. donald
    "QUACK!"

    λ> quack ^. chris
    "Erm, quack?"

Here is a nice thing that we get, we can define any function as taking anything that **has** certain fields, and we know that those fields have the right type because we defined them above:


    > fly :: (Has Flap duck) => duck -> IO ()
    > fly duck = do go; go; go where go = flap ^. duck

    λ> fly donald
    *Flap flap flap*
    *Flap flap flap*
    *Flap flap flap*

We do this multiple times, too:


    > playInPond :: (Has Swim duck, Has Flap duck, Has Eat duck, Has Name duck)
    >            => duck -> Food -> IO ()
    > playInPond duck food = do
    >   putStrLn $ (name ^. duck) ++ " is swimming happily."
    >   swim ^. duck
    >   putStrLn $ "You give them some " ++ show food ++ " to eat."
    >   case (eat ^. duck) food of
    >     Left dnw  -> do putStrLn dnw; fly duck
    >     Right nom -> putStrLn nom
    >   swim ^. duck

And let's see the ducks play in the pond when someone's throwing some food:

    λ> playInPond donald Bread
    Donald Duck is swimming happily.
    *Ducky paddle*
    You give them some Bread to eat.
    Nom nom nom!
    *Ducky paddle*

    λ> playInPond chris Bread
    Chris Done is swimming happily.
    I'm doing the butterfly because it's efficient ...
    You give them some Bread to eat.
    Bread is alright, but no thanks.
    I'm flapping my arms!
    I'm flapping my arms!
    I'm flapping my arms!
    I'm doing the butterfly because it's efficient ...

    λ> playInPond donald Duck
    Donald Duck is swimming happily.
    *Ducky paddle*
    You give them some Duck to eat.
    Are you cwazy!?
    *Flap flap flap*
    *Flap flap flap*
    *Flap flap flap*
    *Ducky paddle*

They both have the same things. However, they *are* different:

    λ> age ^. chris
    67

    λ> age ^. donald

    <interactive>:1:0: No instance for (Contains (Labelled Age Integer)
    TyNil) arising from a use of `^.' at <interactive>:1:0-12

So there you have it, duck typing in a statically typed way. We get to
have our cake and eat it too.

------------------------------------------------------------------------

By the way, and this isn't particularly important, I used a function
to make creating record fields a little nicer, because I don't like
the namelessness of writing `fieldOf 23`:


    > -- | Creation: I like to be able to name the fields that I'm assigning.
    > (^-) :: a -> TypeOf a -> FieldOf a
    > (^-) = const $ fieldOf
    > infixr 6 ^-
