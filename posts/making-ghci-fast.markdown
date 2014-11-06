---
date: 2013-11-20
title: Making GHCi scale better and faster
description: Making GHCi faster
author: Chris Done
tags: haskell, ghci
---

A common complaint with GHCi is that it doesn't scale well when the
size of the project gets bigger. Once you hit 20, 30, 50, or 150
modules, it stops being fun anymore and you start wishing you didn't
have to wait for it to load.

I recommend enabling `-fobject-code`. You can enable this by running

    $ ghci -fobject-code

Or by setting it in the REPL:

    :set -fobject-code

If you want it on all the time, you can put the above line in a
`.ghci` file either in your home directory or in the directory of your
project.

This makes GHCi compile everything once and then use incremental
recompilation thereafter. You'll find that you can load 100-module
projects and work with them just fine in this way.

After that, you may notice that loading some modules gives less type
information and general metadata than before. For that, re-enable
byte-compilation temporarily with `-fbyte-code` (`:set -fbyte-code`)
and `:load` that module again, you now have fast recompilation with
complete information, too.

Another tip is to use `-fno-code` to have _really_ fast
compilation. This also works in combination with `-fobject-code`. But
I'd recommend using this only for type checking, not for getting
useful warnings (like pattern match inexhaustiveness). So I would
combine it with `-fobject-code` in the same way as above with
`-fbyte-code`, and then once you're done hacking, re-enable
`-fobject-code` and rebuild everything.
