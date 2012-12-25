---
date: 2011-10-16
title: “Value polymorphism”, simple explanation with examples
author: Chris Done
tags: haskell, types
---

A concept in Haskell which is particularly novel is that polymorphism
works at the value level rather than function-parameter or
object-dereference level.

Function-parameter polymorphism comes in some different forms, for
example, C++:

    void draw(Circle c){ … }
    void draw(Triangle t){ … }
    draw(circle); // draws a circle

Function overloading is a type of function-parameter
polymorphism. Generic functions in Common Lisp are another way to have
function-parameter polymorphism:

    (defgeneric draw (shape))
    (defmethod draw ((shape circle)) …)
    (defmethod draw ((shape triangle)) …)
    (draw circle) ;; draws a circle

Object-dereference (or message passing) polymorphism is common to most
object oriented languages. Depending on the object, the function/message will
do something different:

    class Circle { void draw(){ … } }
    class Triangle { void draw(){ … } }
    circle.draw(); // draws a circle

To avoid confusion, Haskell also has function parameter
polymorphism:

    class Drawable a where draw :: a -> Bitmap
    instance Drawable Circle where draw = …
    instance Drawable Triangle where draw = …
    draw circle -- draws a circle

Haskell has value polymorphism, which is that any value can be
polymorphic and will be instantiated to a class instance depending on
type signature or annotation:

    class Default a where def :: a
    instance Default Int where def = 0
    instance Default Char where def = 'a'

The type of an expression `def` therefore is `Default a => a`, or,
“any instance of `Default`”. I can instantiate an instance myself by
specifying a type signature:

    λ> def :: Int
    → 0
    λ> def :: Char
    → 'a'

Or by type inference, meaning that the combination of this expression
with other expressions allows the compiler to infer the single correct
type instance:

    λ> def : "bc"
    → "abc"
    λ> def - 2
    → -2
    λ> def == 0
    → True

But with no information it will be a static compile error:

    λ> def
    Ambiguous type variable `a' in the constraint:
      `Default a' arising from a use of `def' at
        <interactive>:1:0-2
    Probable fix: add a type signature that fixes these type
                  variable(s)

Why is value polymorphism beneficial? Some examples follow.

The `Read` class contains a method `read` which is polymorphic on the
return value:

    class Read a where
      read :: String -> a

It parses a data type from a string. Combined with the `Show` class,
together `Read` and `Show` make a naive serialization
library. In the same way, it would be ambiguous to read without
specifying the instance:

    λ> read "2"
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at
        <interactive>:1:0-7
    Probable fix: add a type signature that fixes these type
                  variable(s)

But specifying with a type signature or using type inference are fine:

    λ> read "2" :: Int
    → 2
    λ> read "2" * 3
    → 6

Another example is JSON parsing (the real class is different to this,
but introduces questions that are irrelevant to the point of this
post).

    class JSON a where
      decode :: String -> Result a

The `decode` function is return-value polymorphic, it can be read like this:

    decode :: (JSON a) => String -> Result a

That is, it returns a result (success or fail) with a value which is
an instance of the JSON class.

So both specifying an instance or using inference works:

    λ> decode "1" :: Result Int
    → Ok 1
    λ> do x <- decode "1"; return (x*3)
    → Ok 3

And it works however complex you want to go with your types:

    λ> decode "[[1,\"a\",{\"x\":3}],[1,\"a\",{\"x\":2}]]"
       :: Result [(Int,String,JSObject Int)]
    → Ok [(1,"a",JSONObject {fromJSObject = [("x",3)]})
         ,(1,"a",JSONObject {fromJSObject = [("x",2)]})]

Thus by merely specifying the return type we have effectively
generated a parser. An invalid string will produce an error:

    λ> decode "[[1,\"a\",{\"x\":3}],[1,\"a\"]]"
      :: Result [(Int,String,JSObject Int)]
    → Error "Unable to read Triple"

Similarly I made a URL handling library which can produce a result of
a regex match on that URL:

    class RegexMatch a where
      match :: [String] -> Maybe a

    -- | Match a URL into a value.
    urlMatch :: RegexMatch match => String -> Controller match
    urlMatch regex = do
      uri <- gets controllerURI
      case matchRegex (mkRegex regex) pathPart of
        Just xs -> case match xs of
                     Nothing -> throw $ InvalidRegexFormat regex uri
                     Just x -> return x
        Nothing -> throw $ InvalidURLFormat regex uri
      where pathPart = dropWhile (=='/') $ uriPath uri

For any instances of `RegexMatch` I can say, e.g., for a tuple of `Int`
and `Date`:

    do (user,date) <- urlMatch "([0-9]+)/(.+)$"
       printUserAndDate user date
       …

If the URL is `100/2011/08/12`, `urlMatch` (based on `match`) will parse,
otherwise it will result in `InvalidURLFormat` — it's the user's
fault. If the URL *does* parse, but the regex result does not match
the values we expect (e.g. `(Int,Date)`) then it results in
`InvalidRegexFormat` — it's the programmer's fault.

Such static value polymorphism is difficult to do in popular languages
such as C#, Java, C++, without some kind of proxy objects to
explicitly instantiate an object to dereference using generics or
templates, and hard to do in Python, Ruby and JavaScript without
static type systems.

The list goes on, more examples include database query results,
monads, …

Lastly, the `Default` class is a real class and in common use today.
