---
date: 2011-11-06
title: HaskellDB: A long tutorial
author: Chris Done
tags: haskell, haskelldb
---

I've been using HaskellDB in production for about two years. I decided
that I'd write a proper, up-to-date description, or tutorial, about
what it is, how it works, what it can do, and my experience using it
in projects.[^10]

## What is HaskellDB?

[HaskellDB](http://hackage.haskell.org/package/haskelldb) is a
database interface library for Haskell which features

* explicit declaration of schema of entities and fields,
* an EDSL[^1]—a `Query` monad—for making queries and statements based
  on using operations based on the relational algebra[^4],
* a simple record system for dealing with entities and fields.

It was originally developed for Hugs making use of the TRex, but was
later made portable and is now happy in both Hugs and GHC[^5].

The [Ruby on Rails project](http://rubyonrails.org/) recently adopted
[ARel, a library based on the same
idea](http://github.com/rails/arel).

## Connecting

So to connect to a database with HaskellDB, you can use different
backends. In this case, I'll assume you're using HDBC as a backend.

Pop them in a separate module:

    module Caturday.Model.Connect where

    import Database.HaskellDB.HDBC
    import Database.HaskellDB.Sql.PostgreSQL
    import Database.HDBC.PostgreSQL (connectPostgreSQL)

    withDB :: [(String,String)] -> (Database -> IO a) -> IO a
    withDB opts = hdbcConnect generator (connectPostgreSQL conninfo)
      where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts ]

    opts = [("host","localhost")
           ,("user","your_username")
           ,("password","your_password")
           ,("dbname","your_db_name")]

Usually you ought to combine this with configuration options.

## ORM approach

### Fields

The approach for the object relational mapping is that one defines the
column types and entity schemas up front. So, supposing our project is
named Caturday, in a module named `Caturday.Model.Fields`,[^2] using the
`field` macro,[^3] one declares fields. For example:

    {-# LANGUAGE TemplateHaskell #-}

    -- | All database fields.

    module Caturday.Model.Fields where

    import Database.HaskellDB.TH

    -- Keys.
    field "Id" "id" "id" [t|Int|]

    -- Data fields.
    field "Title" "title" "title" [t|String|]

This produces three things:

* A type called `Title`.
* A field variable called `title`.
* An instance of the field class which can be used to reference
   `title` in the database.

In other words, a Haskell-level representation of the database field
“title”.

### Tables

Then one defines the entities that use those fields in a module such as
`Caturday.Model.Tables`, using the `table` macro[^3]. For example:

    {-# LANGUAGE TemplateHaskell #-}

    -- | Database tables and entities.

    module Caturday.Model.Tables where

    import Caturday.Model.Fields as Fields

    import Database.HaskellDB.TH
    import Prelude ()

    -- | Content table.
    table "content" "content"
      ['id
      ,'title
      ]

The first argument is the Haskell-level value name for the table, and
the second is the SQL-level name of the entity. To import `Fields as
Fields` is a good idea.[^7]

### Importing

Once the model schema has been defined in these two modules, one ought
to import them like so:

    import qualified Caturday.Model.Fields as F
    import qualified Caturday.Model.Tables as T

    import Database.HaskellDB
    import Database.HaskellDB.HDBRec

So that later we will refer to fields as `F.foo` and tables as `T.bar`.

## Querying

### Selection

Queries and statements in HaskellDB are composed with the `Query`
monad. For example, simple selection works like this:

    simpleSelection = do
      table T.content

We use the `table` function to select tables from our predefined set
of tables. It returns a relation of some kind; i.e. tables or result
of a sub-select or projection, etc.

To select more tables (i.e., *joining*) we just list more tables:

    simpleDoubleSelection = do
      table T.content
      table T.content

We execute this query with `withDB`, for example:

    λ> fmap (take 1) $ withDB opts $ \db -> query db simpleSelection
    [[("id","34750"),("title","\"Spanner Based Distributed Channel Assignment in Wireless Mesh Networks\"")]]

### Projection

For projection we use the `project` function defined in
`Database.HaskellDB.Query`:

    simpleProjection = do
      content <- table T.content
      project $ F.id << content!F.id

For each field we want to project, we specify a value for it using the
`(<<)` function, defined in `Database.HaskellDB.Query`. To project
more fields, we use the record constructing operator, `(#)`:

    simpleProjection2 = do
      content <- table T.content
      project $ F.id    << content!F.id
              # F.title << content!F.title

One can see this operator as akin to the tuple constructor `(,)` as in
`(1,2)`.

We can also project our own Haskell-level values as SQL constants:

    simpleStringProjection = do
      content <- table T.content
      project $ F.id    << constant 123
              # F.title << constant "Hello, World!"

Personally in my use, I have renamed `constant` to `val`.[^9] I just find
this more convenient. I will use this henceforth.

    import Database.HaskellDB.Extra

### Restriction

We restrict results using the `restrict` function:

    simpleRestriction = do
      content <- table T.content
      restrict $ content!F.title .==. val "Coco Jambo"
      return content

The restrict function takes an SQL boolean expression:

    λ> :i restrict
    restrict :: Expr Bool -> Query ()
            -- Defined in Database.HaskellDB.Query

For boolean expressions there are a bunch of logical operators that
commonly appear in SQL:

    -- | Equality comparison on Exprs, = in SQL.
    (.==.) :: Eq a => Expr a -> Expr a -> Expr Bool

    -- | Inequality on Exprs, <> in SQL.
    (.<>.) :: Eq a => Expr a -> Expr a -> Expr Bool
    (.<.) :: Ord a => Expr a -> Expr a -> Expr Bool
    (.>=.) :: Ord a => Expr a -> Expr a -> Expr Bool

    -- | \"Logical and\" on 'Expr', AND in SQL.
    (.&&.):: Expr Bool -> Expr Bool -> Expr Bool

    -- | \"Logical or\" on 'Expr'. OR in SQL.
    (.||.) :: Expr Bool -> Expr Bool -> Expr Bool

Note that the convention is for operators in HaskellDB to be
surrounded by periods. For more see the Operators section of
Database.HaskellDB.Query.

### Raw SQL Output

To get a concrete feeling of what SQL this code will produce, let's
observe the output. To do that, we can use `ppSqlUnOpt`:

    λ> :i ppSqlUnOpt
    ppSqlUnOpt :: Query (Rel r) -> Text.PrettyPrint.HughesPJ.Doc
            -- Defined in Database.HaskellDB.PrintQuery

The simple selection:

    λ> ppSqlUnOpt simpleSelection
    SELECT id,
           title
    FROM content as T1

The projection example:

    λ> ppSqlUnOpt simpleProjection
    SELECT id as id1,
           title as title1
    FROM content as T1

The constant value example:

    λ> ppSqlUnOpt simpleStringProjection
    SELECT 123 as id,
           'Hello, World!' as title,
           id1,
           title1
    FROM (SELECT id as id1,
                 title as title1
          FROM content as T1) as T1

The code is clear to read and not that surprising.

### Insertion, delete, update

Insertion uses the `insert` function. Similar to the `query` function,
it takes a connection value. Pretty self-explanatory:

    simpleInsert conn = do
      insert conn
             T.content
             ( F.id    << val 123
             # F.title << val "What is Love?")

The delete function takes a restriction clause for the row to delete,
with the ability to inspect the row for the condition.

    simpleDelete conn = do
      delete conn
             T.content
             (\content -> content!F.title .==. val "Coco Jambo")

Updating is the same as insert, but we provide a function for the
update:

    simpleUpdate conn = do
      update conn
             T.content
             (\content -> content!F.title .==. val "Coco Jambo")
             (\content -> F.title << val "What is Love?")

## Speed and optimisation

But the subquery is useless in this example, so clearly the optimizer
isn't magic.

    λ> ppSqlUnOpt simpleDoubleSelection
    SELECT id2 as id,
           title2 as title
    FROM (SELECT id as id2,
                 title as title2
          FROM content as T1) as T1,
         (SELECT id as id1,
                 title as title1
          FROM content as T1) as T2

In fact, subqueries are created in all cases.

For normal query optimizers, e.g. PostgreSQL, the subquery is lifted
as to be equivalent to there being one query. I am not sure about
MySQL; it may have trouble when joins are involved. Don't expect good
performance from HaskellDB if you're using MySQL.[^11]

For example, PostgreSQL sees such use of sub-query as equivalent to
direct join:

    => explain SELECT id2 as id,
               title2 as title
        FROM (SELECT id as id2,
                     title as title2
              FROM content as T1) as T1,
             (SELECT id as id1,
                     title as title1
              FROM content as T1) as T2;
                              QUERY PLAN
    ------------------------------------------------------------------
     Nested Loop  (cost=1578.48..330231.64 rows=16353936 width=77)
     ->Seq Scan on content t1 (cost=0.00..1574.44 rows=4044 width=77)
     ->Materialize (cost=1578.48..1618.92 rows=4044 width=0)
       ->Seq Scan on content t1 (cost=0.00..1574.44 rows=4044 width=0)
    (4 rows)

    => explain SELECT T1.id as id,
               T1.title as title
        FROM content as T1,
             content as T2;
                              QUERY PLAN
    -------------------------------------------------------------------
     Nested Loop  (cost=1578.48..330231.64 rows=16353936 width=77)
     ->Seq Scan on content t1 (cost=0.00..1574.44 rows=4044 width=77)
     ->Materialize (cost=1578.48..1618.92 rows=4044 width=0)
       ->Seq Scan on content t1 (cost=0.00..1574.44 rows=4044 width=0)
    (4 rows)

    =>

I'm not joining on any indexes so it's a sequence scan. For people not
used to PostgreSQL output, this basically means it will do a
cartesian product in both versions.

## Maintenance

The great part about HaskellDB is that it is in first-class Haskell
land. Fields and tables have a statically enforced membership and
field-type schema.

The obvious use case is that it avoids making mistakes in naming and
ending up with the wrong field type, or using a field that doesn't
exist in a given table.

The fact that all fields are defined up front with the right type
means that one really has to think about how meaningful a type is and
how one will use it. For example:

    field "Abstract" "abstract" "abstract" [t|Maybe String|]

This is how to encode a database text field that is nullable. When one
is encoding their database schema into the Haskell type system, one
finds that it really needs to be thought of properly of what types are
there in the database, particularly nullability.

In my day to day work, I have to work with database schemas that
aren't mine, I have to interface with them. Due to my use of
HaskellDB, I have a lot of correctness questions about these schemas
I'm working with to the authors, if they are available for
consultation.

Often it comes up, that I ask “why is this field nullable?” and the
question often comes back, “I don't know.” As the PostgreSQL
documentation says, in most database designs the majority of columns
should be marked not null.[^12]

Note that in Haskell nullability is not implicit. No values can be
null. But you can have choice between a value or not a value, as in
`Maybe`:

    data Maybe a = Just a | Nothing

And so if we use the abstract field, as mentioned, and use it as a
string, it's not a string, it's a `Maybe String`, so we get a compile
error such as:

    Mismatch: Demo.hs:23:32: “Maybe String” ≠ “String”

Another nice property is that fields named in your codebase, and their
names in the database, are entirely separate and configurable. Just
because Joe Master Designer chose certain names in his schema, that
doesn't mean that you have to conform to those names. Maybe they call
it `thetitle`, and you just want `title`:

    field "Title" "title" "thetitle" [t|String|]

Another fact is changes to the schema underneath: if someone (you or
someone else) changes the type or availability of a field or table in
the schema, all you need do is make the necessary change in the field
module or table module, and the compiler will tell you immediately
which modules need updating with the new invariants.

Suppose we change the type of the field title to `Int` (for example),
when we recompile our examples above, we get:

    Mismatch: Demo.hs:23:32: “Int” ≠ “String”
    Mismatch: Demo.hs:25:0: “String” ≠ “Int”

So the following two functions are now inconsistent:

    simpleStringProjection = do
      content <- table T.content
      project $ F.id    << constant 123
              # F.title << constant "Hello, World!"

    simpleRestriction = do
      content <- table T.content
      restrict $ content!F.title .==. val "Coco Jambo"
      return content

In a codebase of 10K+ lines, this starts to become very compelling.

I believe LINQ in C# et al provide similar static assurances.

## Extension

### Pagination and composing queries

Because the query DSL is a monad (as plenty of Haskell DSLs are), it
is really nicely composable. This means it's trivial to split up
queries into discrete parts that have meaningful and generic purposes.

For example, to implement pagination, which is essentially the simple
problem of an offset and a count. I implemented this in
`HaskellDB.Database.Pagination`.[^13]

Thus the following implementation is possible. Suppose we write some
functions to search the articles by title in the database, but
paginated. Two things we need for this are:

* The count of total articles filtered by the search query, `q`.
* The paginated articles filtered by the search query.

First we define a query that does the search and restriction:

    getContent q = do
      article <- table T.content
      restrict $ article!F.title .==. val q
      return article

In the fields module we need another field:

    field "Count" "count" "count" [t|Int|]

Then we write the function that uses this query, and projects the
count:

    getArticlesCount conn q = fmap (sum . map (!F.count)) $ query conn $ do
      article <- getContent q
      project $ F.count << count (article!F.id)

Then we can write a function to get the articles and then paginate.

    getArticles conn pn q = fmap (map (!F.title)) $ query conn $ do
      article <- getContent q
      paginate pn
      return article

Very, very easy to compose.

### Functions and operators

Sometimes you want to define more SQL functions and operators, which
is
[a use case I had for PostgreSQL](http://hpaste.org/53608) as
`Database.HaskellDB.PostgreSQL`.

The function to use for extending with new functions is `func`.

    λ> :i func
    func :: (Args a) => String -> a
            -- Defined in Database.HaskellDB.Query

`Args` is implemented as a way to have arbitrary number of
serializable parameters, in the same way `Text.Printf.printf` works.

For example,
[date_part](http://www.postgresql.org/docs/8.1/static/functions-datetime.html)
in PostgreSQL:

    -- | Get part of a date.
    date_part :: Expr String -> Expr CalendarTime -> Expr Integer
    date_part = func "date_part"

Or maybe we want to use full text search support from
PostgreSQL. Let's add a field to represent the `ts_vector`, and define
a table with the searchable stuff:

    -- | Search fields.
    field "Textsearchable" "textsearchable" "textsearchable_index_col"
          [t|TSVector|]

    -- | Content table with searchable full text field.
    table "contentSearchable" "content"
      ['id
      ,'title
      ,'textsearchable
      ]

Now we can redefine `getContent` which matches on the `ts_vector`:

    getContentFullText q = do
      article <- table T.contentSearchable
      restrict $ article!F.textsearchable .@@. (to_tsquery (val q))
      order [descExpr $ ts_rank_cd (article!F.textsearchable)
                                   (to_tsquery (val q))]
      return article

This can be achieved by a phantom type (i.e. an uninhabitable type at
the value-level):

    -- | A text-search vector.
    data TSVector

    -- | Convert a string to a textsearch vector.
    to_tsvector :: Expr String -> Expr TSVector
    to_tsvector = func "to_tsvector"

And the function can be used, at the SQL-level, because the vector is
constructed at the SQL-level, not the Haskell level. That's quite
nice.

Likewise, `descExpr` was written by me from the
Database.HaskellDB.Extra module:

    -- | Order the query by an arbitrary expression.
    descExpr :: Expr a -> OrderExpr
    descExpr e = OrderExpr OpDesc expr where
      Expr expr = e

One just needs access to the internal tree and a new combinator can be
constructed.

## Enums

It's also possible to use enum types with HaskellDB that map from
DB-level enums and Haskell enums.

Suppose we define an Enums module, with an enum type `ArticleType`:

    module Caturday.Types.Enums where

    import Data.Enum.Print
    import Database.HaskellDB.Get
    import Database.HaskellDB.PrimQuery
    import Database.HaskellDB.Query

    data ArticleType
      = ResearchArticle
      | Editorial
      deriving (Show,Enum,Read,Eq)

In order to get values of this type from the database with HaskellDB,
we need to implement the `GetValue` and the `ShowConstant` classes:

    instance GetValue ArticleType where
      getValue = getValueWith readEnum "ArticleType"

    instance ShowConstant ArticleType where
      showConstant = StringLit . showEnum

This uses two *other* modules (I know, I'm referencing a lot of
modules I've written, sorry),
[Data.Enum.Print](http://hpaste.org/53612), a simple enum
serialization module, and
[Database.HaskellDB.Get](http://hpaste.org/53614), which provides the
`getValueWith` function.

Now we can define the fields that use this type in our Fields module:

    import qualified Caturday.Types.Enums as Types (ArticleType)

    -- | Enum types.
    field "ArticleType" "articleType" "type" [t|Types.ArticleType|]

and redefine the content table's schema:

    -- | Content table.
    table "content" content"
      ['id
      ,'title
      ,'articleType
      ]

So now, our Demo module doesn't compile, so we update:

    import Caturday.Types.Enums

    simpleInsert conn = do
      insert conn
             T.content
             ( F.id          << val 123
             # F.title       << val "What is Love?"
             # F.articleType << val Editorial)

And we can also use the enum for restriction as well as retrieval:

    restrictOnArticleType conn = query conn $ do
      content <- table T.content
      restrict $ content!F.articleType .==. val Editorial
      return content

    λ> fmap (take 1) $ withDB opts $ \db -> restrictOnArticleType db
    [[("id","82")
    ,("title","\"Welcome message from the Editor-in-Chief\"")
    ,("type","Editorial")]]

## Stability

The problem with HaskellDB is that the implementation can be
unstable. I found that I had to patch the PostgreSQL library to handle
simple stupid things like fields named "user" or "order", by making
sure to quote all fields.

I also had to open up some of the internal parts of the API so that I
could extend it further, such as for the operator `(.@@.)` defined
above. I'll push these fixes and extensions to fork repos at some
point.

## Reading error messages

HaskellDB gets a lot of stick for hard to read error
messages. This is true when you get things badly wrong.

In the general case the errors are quite straight forward.

For example, if I try to use a field which doesn't exist in the table,
like this:

    simpleRestriction = do
      content <- table T.content
      restrict $ content!F.count .==. val 1
      return content

Then the compile error looks like this:

    Error: Demo.hs:39:13: No instance for (HasField F.Count RecNil)
          arising from a use of `!' at Demo.hs:39:13-27
        Possible fix:
          add an instance declaration for (HasField F.Count RecNil)
        In the first argument of `(.==.)', namely `content ! F.count'
        In the second argument of `($)', namely
            `content ! F.count .==. val 1'
        In a stmt of a 'do' expression:
              restrict $ content ! F.count .==. val 1

Which is a very useful error message. `content` does not has field
`count`.

For getting the wrong type, it merely shows “couldn't match type A
against type B,” straight-forward.

The cases where compile errors blow up are, for example, if I wrote
this:

    simpleRestriction = do
      content <- table T.content
      restrict $ content!content .==. val "Coco Jambo"
      return content

    then the error is:

    Error: Demo.hs:39:13:
      No instance for
        (Select
          (Rel
            (RecCons F.Id (Expr Int)
                    (RecCons F.Title (Expr String) RecNil)))
          (Rel
            (RecCons F.Id (Expr Int)
                    (RecCons F.Title (Expr String) RecNil)))
          (Expr [Char]))
          arising from a use of `!'
                       at Demo.hs:39:13-27
        Possible fix:
          add an instance declaration for […]
        In the first argument of `(.==.)', namely `content ! content'
        In the second argument of `($)', namely
            `content ! content .==. val "Coco Jambo"'
        In a stmt of a 'do' expression:
              restrict $ content ! content .==. val "Coco Jambo"

The error actually makes sense if you understand the API well enough,
but otherwise it can be very confusing and worrying. Don't worry
about it, you didn't break something complicated, you just made a typo
somewhere. It shows the offending expression; you realised you tried
to use a table as a field, and you correct.

## Files

The complete files for this demo including patched libraries are [here
in Github flavour.](https://github.com/chrisdone/haskelldb-demo)

[^1]: Embedded domain-specific language. A common notion in Haskell
      and Lisp languages, though implemented differently in each.

[^2]: This is the convention I have chosen to use. It makes good sense
      and can be very helpful for all fields used in the project to be
      defined on a per-project basis, rather than per-entity, and of
      the same type.

[^3]: A macro that you can get from
      [Database.HaskellDB.TH](http://hpaste.org/53595), which I have
      yet to put into a library or get added to HaskellDB mainline. I
      don't care to debate API decisions with the HaskellDB
      maintainers right now.

[^4]: See [package description for
      HaskellDB](http://hackage.haskell.org/package/haskelldb).

[^5]: See the [“HaskellDB Improved”
      paper.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.136.3828&rep=rep1&type=pdf)

[^7]: When table names conflict with field names—and eventually it
      happens—this is useful to have. Alternatively `as F` also makes
      sense, to be consistent.

[^9]: I have defined a few extra functions for HaskellDB in
      [Database.HaskellDB.Extra](http://hpaste.org/53599).

[^10]: Afterwards it would seem like a good idea to get a proper
       comprehensive tutorial on the HaskellWiki, or much better yet,
       embed a tutorial in the Haddock documentation for HaskellDB. At
       the moment the haddock docs are literally just an API listing,
       with no elaborative explanation or examples. Writing in Haddock
       mark-up is quite a painful, boring experience. Regardless, I
       believe the haddock docs of a project should (most of the time)
       be sufficient to explain its use, linking to external papers
       and blog posts and whatnot is annoyingly terse and quickly
       becomes out of date.

[^11]: [Geoff Wilson on HaskellDB performance.](http://pseudofish.com/blog/2008/05/18/haskelldb-performance/)

[^12]: [PostgreSQL manual on constraints.](http://www.postgresql.org/docs/8.3/static/ddl-constraints.html)

[^13]: [Another simple module](http://hpaste.org/53619) and
       [another](http://hpaste.org/53607) that would be cool to put
       in a package, but for now remains in my utility box.
