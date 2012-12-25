---
date: 2012-01-06
title: Compiling to JavaScript with UHC
author: Chris Done
tags: haskell, javascript, uhc
---

Last week I took a crack at testing the JavaScript compilation backend
supported by UHC. There don't seem to be any instructions[^1], that I
have been able to find, on working with the JS backend. But it's more
or less straight-forward to figure out.

As far as I understand quite a lot of what's working now is thanks to
Atze Dijkstra and Jurriën Stutterheim,[^5] so cheers to those!

# Build and Install UHC

**UPDATE 2012-04-23**: The Git repo has moved to:
  git://github.com/UU-ComputerScience/uhc.git

I used the UHC repository from Github:

    $ mkdir uhcjs
    $ cd uhcjs
    $ git clone git@github.com:uhc/uhc.git
    $ cd uhc/EHC

GHC 7+ should work okay, I imagine.[^3]

There is no INSTALL file and the README doesn't mention installation,
but the configure script is helpful.

    $ ./configure

Have a look, see if it mentions that you need something installed.[^2]
Personally, I had to install a few:

    $ cabal install uuagc fgl EH-Util syb

Then I just ran make (`-j` runs n jobs and I have four cores):

    $ make uhc -j 4
    …
    real	4m40.388s
    user	6m19.628s
    sys	0m12.053s

Takes about 5 minutes on a modern machine,[^4] so you probably want to run
both make and make install (sans `-j`), which takes about 4 minutes:

    $ make uhc -j 4 && sudo make install

And go get a cup of tea.

    $ uhc --version
    ehc-1.1.2, Revision exported

Now UHC is installed.

# Get the JavaScript library

**UPDATE 2012-02-16**: Git repo below is currently not working due to
  rename from jscript → js, use
  git://github.com/spockz/uhc-jscript.git instead.

    $ cd ../..
    $ git clone git://github.com/norm2782/uhc-jscript.git

There is a Cabal file which does configure and build, but I couldn't
figure out how it was supposed to link up the JavaScript dependencies
and generate a HTML file (that worked), but a direct call to uhc
works.

# First program

 Go to the src directory:

    $ cd uhc-jscript/uhc-jscript/src

Open up `Main.hs` and replace everything with something nice like:

    import Language.UHC.JScript.Assorted
    main = alert "Hello, Haskell!"

Then run:

    $ uhc -tjs Main.hs

And open `Main.html` in your browser.

That's about as far as I got. There's a fairly substantial API already
defined by Jurriën Stutterheim in src/, e.g. a
[HTML5 API](https://github.com/norm2782/uhc-jscript/blob/master/uhc-jscript/src/Language/UHC/JScript/W3C/HTML5.hs)
and
[jQuery](https://github.com/norm2782/uhc-jscript/blob/master/uhc-jscript/src/Language/UHC/JScript/JQuery/JQuery.hs)
and
[AJAX](https://github.com/norm2782/uhc-jscript/blob/master/uhc-jscript/src/Language/UHC/JScript/JQuery/Ajax.hs). Enjoy
hacking! It's more complete than GHCJS, so it may be an interesting
route to take for the Ludum Dare for making a browser-based game.

[^1]: Bar a [short mention](http://www.cs.uu.nl/wiki/bin/view/Ehc/UhcUserDocumentation#5_7_3_jscript_Core_based_JavaScr)
      in the user manual.

[^2]: For example:

        Required:
          uuagc is available?     : yes
            version               : Attribute Grammar compil…
            command for uuagc     : /home/chris/.cabal/bin/uuagc
          ghc is available?       : yes
            version               : 6.12.3
            command for ghc       : /home/chris/Programs/bin/ghc
            command for ghc raw   : /home/chris/Programs/bin/ghc
            libraries:
              uulib is available? : yes
                version           : uulib-0.9.14

[^3]: I had to use GHC 7.0.4 as GHC 6.12.3 failed with a rather unsightly
plethora of linker errors. So I enabled GHC 7.0.4:

        $ export PATH=/opt/ghc/7.0.4/bin/:$PATH

[^4]:  Intel(R) Core(TM) i7 CPU M 620 (2.67GHz and 4096 KB cache size)
       x× 4.

[^5]: Check [the paper on improving the UHC backend](http://www.norm2782.com/improving-uhc-js.pdf).
