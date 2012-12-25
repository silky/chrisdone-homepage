---
date: 2010-10-07
title: HaskellDB and TypeOperators madness
description: HaskellDB and TypeOperators madness
author: Chris Done
tags: haskell, haskelldb
---

## HaskellDB

So in HaskellDB, there is a nice, clever record system which is
used to express tables, queries, projections, etc. -- anything
that has fields and types related to them. It's kind of similar
to Hugs's TRex record system, but invented to work with plain
Haskell and, most importantly, in GHC.

The idea behind HaskellDB is that you generate these records for
your tables from your database schema. But I've found that
sometimes you don't want all fields from the schema, or that you
want to operate on a projection. All the examples of HaskellDB,
and there aren't many, don't have explicit type signatures on the
functions that operate on records, and that's a good thing,
because if they did, it would be a huge ugly verbose signature.

I wanted to:

1. Make defining entities trivial.
2. Make annotating my functions that work on them not verbose and
   ugly.

## HaskellDB works like this

Entities in HaskellDB are typed like this:

    type Person = RecCons Name (Expr String)
                   (RecCons Age (Expr Integer)
                    (RecCons Email (Expr String)
                     (RecCons Blurb (Expr (Maybe String))
                      RecNil)))

It's sort of like a heterogenous list type but restricted to
instances of `FieldTag`. RecCons describes the field,
e.g. `Name`, which is a data type implementing the `FieldTag` class,
e.g.:

    data Name = Name
    instance FieldTag Name where fieldName _ = "name"

This allows HaskellDB to associate this field with an actual
field in the database by its schema name `name`. And then for
writing actual fields of a record, there's usually a value that
goes with it:

    name :: Attr Name String
    name = mkAttr Name

Which describes that its field is `Name` and the corresponding
type is `String`. The type of `mkAttr` is:

    mkAttr :: (FieldTag f) => f -> Attr f a

Maybe I should write an up-to-date explanation of how HaskellDB
works from the bottom up. It's really simple but when I first
approached it I thought it was a bit magic and over the top, and
didn't enjoy it. The documentation available isn't great or is
out of date, and the paper, although great, uses Hugs's TRex and
is kind of unfamiliar. But I'll leave that for another time, I'll
probably do a write-up on the HaskellWiki.

## Enter type operators!

Anyway, to make my life a bit easier, I defined the following type:

    type F a b c = RecCons a (Expr b) c

Now, I could define entities in a way that wasn't so ugly:

    type Person = (F Id Integer
                   (F Age Integer
                    (F Email String
                     (F Blurb (Maybe String)
                       RecNil))))

And it feels a bit more efficient, but still kind of clunky and
obvious that I'm manually constructing a list and balancing
parentheses.

I found that if I define this helper type operator,

    type a :=>: b = RecCons a b
    infixr 2 :=>:

then I can make this nice type operator to define a type for a record:

    type a :<: b = a :=>: (Expr b)
    infixr 2 :<:

And then combine records together with this operator,

    type a :+: b = a b
    infixr 1 :+:

and with an aesthetic type to complete the abstraction:

    type End = RecNil

Now I've got a more natural, more familiar way of writing
records:

    type Person = Id    :<: Integer
              :+: Age   :<: Integer
              :+: Email :<: String
              :+: Blurb :<: Maybe String
              :+: End

Isn't that nice!

## Parametrizing the record

This is pretty good, but I have another problem. I want to be
able to define an entity which is parametrized by a type
function over the type of the field. E.g. the `person` entity's
fields are wrapped by the `Expr` type function. The above is
really:

    type Person = RecCons Name (Expr String)
                   (RecCons Age (Expr Integer)
                    (RecCons Email (Expr String)
                     (RecCons Blurb (Expr (Maybe String))
                      RecNil)))

`Expr` is used when we're constructing a query or restriction for
an entity. But when we have the actual record, i.e. from a
result, the type is:

    type Person = RecCons Name String
                   (RecCons Age Integer
                    (RecCons Email String
                     (RecCons Blurb (Maybe String)
                      RecNil)))

If I want to annotate my top-level functions which work on these
types, I've got to write it out every time ... Or I can parametrize
the type:

    type Person a = RecCons Name (a String)
                     (RecCons Age (a Integer)
                      (RecCons Email (a String)
                       (RecCons Blurb (a (Maybe String))
                        RecNil)))

Now I can have:

    type PersonQuery = Person Expr

And with a type-level id function,

    type TId a = a

I can have:

    type PersonRow = Person TId

So to deal with this extra notion in my little set of type
operators, I defined another operator:

    type a :~>: f = f a
    infixr 3 :~>:

Now I can define `Person` like this:

    type Person a =
          Id    :=>: Integer      :~>: a
      :+: Age   :=>: Integer      :~>: a
      :+: Email :=>: String       :~>: a
      :+: Blurb :=>: Maybe String :~>: a
      :+: End

This is okay! It's still a bit verbose, though. I don't like the fact I
need to mention the `a` for every single field. It's practically
a type-level reader/environment monad.

## Type-level monad, kinda ...

Here's what I came up
with:

    type (c :~~>: b) a k = (RecCons c (a b)) k
    infixr 2 :~~>:

`(:~~>:)` is kind of a binary `return` function, because it
constructs a record from a field type `c` and value type `b`, and
then takes an `a` which is the environment, and corresponds to
the `a` that we duplicated n times above in `Person`. Finally it
takes another field, `k`. GHCi tells us:

    λ> :k (:~~>:)
    (:~~>:) :: * -> * -> (* -> *) -> * -> *

Which makes it very clear.

Then I need a way to combine fields which doubles up as a `(>>)`
function for the types, passing the `a` type to each field
("monadic type"):

    type (x :>>: y) (a :: * -> *) = x a (y a)
    infixr 1 :>>:

A kind-of bind type function. GHCi again gives insight as to the
meaning of this:

    λ> :k (:>>:)
    (:>>:) :: ((* -> *) -> * -> *) -> ((* -> *) -> *) -> (* -> *) -> *

Which can be read as taking a record constructor, that's

    ((* -> *) -> * -> *)

and a record to combine it with that doesn't take a record
constructor (this is analogous to x : y in combining lists):

    ((* -> *) -> *)

And then finally take the `a` type which is merely `(* -> *)`.

To help create a `((* -> *) -> *)` type, which is basically the
same as the first parameter to the type, but ignores the `a`
environment variable, I define a type-level `const`:

    type Const c a = c

I don't really have to explain the use much, the kinds tell
everything:

    λ> :k Const End
    Const End :: (* -> *) -> *

**Note:** Paul Graphov kindly pointed out to me that in my Person
  examples below I had put `End` instead of `Const End`, which was
  incorrect and confusing. Thank you, Paul.

Now I can combine the record with the final nil record, and I
have a kind of monad-ish type that accepts an environment
variable:

    λ> :k (Id :~~>: Integer) :>>: (Const End)
    (Id :~~>: Integer) :>>: (Const End) :: (* -> *) -> *

I can provide it by merely applying it:

    λ> :k ((Id :~~>: Integer) :>>: (Const End)) TId
    ((Id :~~>: Integer) :>>: (Const End)) TId :: *
    λ> :k ((Id :~~>: Integer) :>>: (Const End)) Expr
    ((Id :~~>: Integer) :>>: (Const End)) Expr :: *

Done! The type is sound. I think that's pretty nice!

I can now define my parametrized table:

    type Person a =
          (Id    :~~>: Integer
      :>>: Age   :~~>: Integer
      :>>: Email :~~>: String
      :>>: Blurb :~~>: Maybe String
      :>>: Const End) a

Sweet!

Then I went a bit type operator crazy and defined a type-level
`flip ($)` and `($)`:

    type a :%: f = f a
    infixr 0 :%:

    type f :$: a = f a
    infixr 0 :$:

So that I could write:

    type PersonTable = Table :$: Expr
      :%:  Id    :~~>: Integer
      :>>: Age   :~~>: Integer
      :>>: Email :~~>: String
      :>>: Blurb :~~>: Maybe String
      :>>: Const End

## The end result, lessons learned

Suddenly type-annotating my functions that work on HaskellDB's
records seems a lot more attractive! Shortly after, I redefined
the operators to be more specific to the record idea, and changed
them to:

    type PersonTable = Table :$: Expr
       :%: Id    ::: Integer
       :+: Age   ::: Integer
       :+: Email ::: String
       :+: Blurb ::: Maybe String
       :+: Const End

Which I think reads pretty nicely. Kind of like Haskell's native
records. Have I gone type operator mad?  Maybe, but I love that I
can define them. And I didn't have to change the HaskellDB
library one bit!

And kinds are surprisingly easy to grok with the GHCi `:k`
command! I stepped through, checking my type-expressions on the
way, just like I do with types and checking my
value-expressions. I did try defining a `flip`, which works:

    type Flip (f :: (* -> *) -> * -> *) a b = f b a

But it's not polymorphic on the parameters of `f`. Kind polymorphism
would be neat, but I wonder how much use it would have. It we had
it, though, this "reader monad" could be parameterized over an
arbitrary kind.
