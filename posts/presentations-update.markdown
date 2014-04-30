---
date: 2014-04-30
title: Presentations update
description: Presentations update
author: Chris Done
tags: haskell, emacs
---

Just a small update. I took 15 mins and updated the haskell-mode
printer a bit so that everything is indented by default, and lists are
expanded as `[1,2,…]` rather than `1:(2:…)`.

[Video demonstration!](https://www.youtube.com/watch?v=oJhIvHtflbI)

Andrew Gibiansky contacted me about getting a front-end added for
[IHaskell](https://github.com/gibiansky/IHaskell), which would be
lovely! I designed the [present](https://github.com/chrisdone/present)
package specifically aimed at working on Emacs or the browser or
wherever. So I sent him back an excessively long email about how to
integrate it.

It might also be worth adding to tryhaskell, too. It'd be rather easy
and helpful to newbies.

Also, update to the loeb example:

``` haskell
λ> :present loeb (map (\i l -> Node i (map (fmap (+1)) l)) [1..3])
[Node
   1
   [Tree Integer
   ,Node
      3
      [Node
         3
         [Tree Integer
         ,Tree Integer
         ,Node
            6
            [Node
               5
               [Node
                  6
                  [Tree Integer
                  ,[Tree Integer]]
               ,[Tree Integer]]
            ,[Tree Integer]]]
      ,[Tree Integer]]
   ,Tree Integer]
,Tree Integer
,Tree Integer]
```

You can really drill down into those structures!
