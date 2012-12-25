---
date: 2012-01-31
title: Currying vs partial application
description: Currying vs partial application
author: Chris Done
tags: javascript
---

If you're looking for an explanation of currying and partial
application in JavaScript, I wrote one here.

In lambda calculus (bear with me) all functions take one argument, but
you can invent some syntactic sugar to express a conceptual
multi-argument function,

    foo = λx y z. x * y * z

which can be transformed (curried) to

    bar = λx. λy. λz. x * y * z

therefore it can be trivially partially applied (where “→” reads “evaluates to”)

    bar 1 2 → λz. 1 * 2 * z

and finally

    bar 1 2 3 → 6

the same is true for Haskell and ML. In JavaScript, however, all
functions take any number of arguments, so you have syntax to express
that already,

    var foo = function(x,y,z){ return x * y * z; };

which can be curried (implementing the `curry` function can be a fun
exercise for the reader) like this,

    var bar = function(x){ return function(y){ return function(z){ return x * y * z; }; }; };

and therefore can be trivially partially applied

    bar(1)(2) → function(z){ return 1 * 2 * z; };

and finally

    bar(1)(2)(3) → 6

However, JavaScript functions can be partially applied without
currying, too, with a simple implementation like

    var partially_apply = function(){
      var self = this;
      var func = arguments[0];
      var original_args = Array.prototype.slice.call(arguments,1);
      return function(){
        return func.apply(self,original_args.concat(Array.prototype.slice.call(arguments)));
      }
    }

in other words, take a function and some args to apply partially, and return a
function that takes the rest of the arguments, so you can partially
apply like,

    partially_apply(foo,1,2) → function(){
      return func.apply(self,[1,2].concat(Array.prototype.slice.call(arguments)));
    }

or semantically

    partially_apply(foo,1,2) → function(z){ return 1 * 2 * z; };

and finally

    partially_apply(foo,1,2)(3); → 6

or if `partially_apply` is renamed to something more pretty, like `$$`,

    $$(foo,1,2)(3); → 6

This can be very convenient for avoiding explicit anonymous functions, e.g.

    var add = function(x,y){ return x + y; }

    [1,2,3,4].map($$(add,5)); → [6,7,8,9]

Thus we have shown currying is not necessary for partial application
with sufficient reflection.
