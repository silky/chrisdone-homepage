---
date: 2011-04-10
title: ‘amb’ operator and the list monad
description: ‘amb’ operator and the list monad
author: Chris Done
tags: haskell, designs
---

A friend was
[messing about with the `amb` operator](http://mihai.bazon.net/blog/amb-in-javascript/take-two)
in JavaScript after seeing it in Common Lisp.  The `amb` (or
*ambiguous*) operator, first described by our pal John McCarthy
(1967), and something I first
[encountered in SICP.](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3)

These kind of constraint/logic puzzles seem naturally solved by the
list monad,
[here's an example of solving ‘Who owns the fish?’](http://hpaste.org/45504/who_owns_the_fish)
Similar to
[the Zebra puzzle.](http://en.wikipedia.org/wiki/Zebra_Puzzle)


    -- Translation of
    -- http://mihai.bazon.net/blog/amb-in-javascript/take-two#wotf

    import Control.Monad
    import Data.Maybe
    import Data.List

    whoOwnsTheFish = addHouse []

    addHouse houses = do

      nat <- other "nat" ["British","Swedish","Danish","Norwegian"
                         ,"German"]
      col <- other "col" ["red","green","white","yellow","blue"]
      pet <- other "pet" ["dogs","cats","horses","birds","fish"]
      bev <- other "bev" ["tea","milk","coffee","beer","water"]
      tob <- other "tob" ["pallmall","dunhill","marlboro"
                         ,"winfield","rothmans"]

      (nat == "British")  `iff` (col == "red")
      (nat == "Swedish")  `iff` (pet == "dogs")
      (nat == "Danish")   `iff` (bev == "tea")
      (col == "white")    `iff`
        (thisHouse > 0 && "col" `lookup` (houses!!(thisHouse - 1))
                           == Just "green")
      (col == "green")    `iff` (bev == "coffee")
      (tob == "pallmall") `iff` (pet == "birds")
      (col == "yellow")   `iff` (tob == "dunhill")
      (thisHouse == 2)    `iff` (bev == "milk")
      (thisHouse == 0)    `iff` (nat == "Norwegian")
      (tob == "winfield") `iff` (bev == "beer")
      (nat == "German")   `iff` (tob == "rothmans")

      let h = [("nat",nat),("bev",bev),("tob",tob),("pet",pet)
              ,("col",col)]
          a = houses ++ [h]
      if length a == 5
         then do neighbors a "tob" "marlboro"  "pet" "cats"
                 neighbors a "pet" "horses"    "tob" "dunhill"
                 neighbors a "nat" "Norwegian" "col" "blue"
                 neighbors a "tob" "marlboro"  "bev" "water"
                 return a
         else addHouse a

      where other typ = filter (isNothing . findHouse houses typ)
            thisHouse = length houses

    findHouse houses typ val =
      fmap fst . find ((==Just val) . lookup typ . snd) . zip [0..]
       $ houses

    neighbors houses typ1 val1 typ2 val2 = guard $ diff == Just 1
      where diff = do h1 <- findHouse houses typ1 val1
                      h2 <- findHouse houses typ2 val2
                      return $ abs $ h1 - h2

    iff x y = guard $ x == y
