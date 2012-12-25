---
date: 2011-08-20
title: Experimenting with GHCJS, the Haskell→JavaScript compiler
description: Experimenting with GHCJS, the Haskell→JavaScript compiler
author: Chris Done
tags: haskell, javascript, web
---

<!-- Part 1: Explain what's wrong with JavaScript -->

JavaScript *per se* is insufficient. The depths to which JavaScript
fails is well-documented and well-understood. Its main faults are its
verbose function syntax[^13], late binding[^1], which has
led to the creation of various static analysis tools to alleviate this
language flaw[^2], but with limited success[^3] (there is even a
static type checker[^4]), [finicky
equality](http://stackoverflow.com/questions/5447153/javascript-equality-transitivity-is-weird/5447170#5447170),
`this` behaviour, and lack of static types and modules[^12].

<!-- Part 2: Explain the ways in which we fix JavaScript -->

Using JavaScript for what it is good for[^6], but not using the
language *per se*, is therefore desirable, and many are working to
achieve this[^5], in some form or another. There various ways to do
it[^7], but I will opt for compiling an existing language, Haskell, to
JavaScript, because I do not have time to learn or teach other people
a new language, garner a new library set and a new type checker and
all that Haskell implementations provide.

<!-- Part 3: Explain compiler choice -->

Given the option, I’d choose [GHC](http://www.haskell.org/ghc/)
because it is the flagship Haskell compiler, with [the most
features](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghc-language-features.html),
which most Haskellers write all their projects with. Haskell has a
standard, [presently Haskell
2010](http://www.haskell.org/pipermail/haskell/2009-November/021750.html),
but I think that most projects out there use quite a few language
extensions[^8] outside of the standard; Haskellers target GHC. This is
not to say that for compiling to JS, Haskell 98 wouldn't be a vast
improvement.

<!-- Part 4: Introduce GHCJS -->

Fortunately there is a project maintained by [Victor
Nazarov](http://asviraspossible.livejournal.com/) called
[GHCJS](https://github.com/sviperll/ghcjs). You can use GHC 6.12.3 or
GHC 7+. For my experimentation I am using 6.12.3. I followed [the
instructions
given](https://github.com/sviperll/ghcjs/blob/master/README.markdown),
with a fix for the build process[^9], and some tweaks to the
libraries[^10]. In order to build the libraries and copy them to the
`examples/` directory, I wrote a little script[^11], which helps
automate this. There is also `BuildTest.hs` in the `examples/` dir
which gentle reader should try first.

<!-- Part 5: First GHCJS example -->

After much twiddling and fudging with the example file and the
provided FFI, some help from Victor Nazarov, with some trial and
error, I managed to get some fundamental things working that are
necessary to be able to write effectively in the JavaScript
environment[^14]. Timers work (and AJAX requests will), but
[this example](http://hpaste.org/50477#line128) is merely a clickable
blank page which alerts “‘Ello, World!”. Uninteresting functionally,
but a good test of the fundamentals (see the pasted Haskell source).

<!-- Part 6: Where next -->

Next up, I will write a simple pong game[^15] to test integration with the
canvas element and speed of the runtime and establish some sort of
base library and project template from which other Haskellers can more
easily experiment. Perhaps we could even have in the future a
browser-based IDE and compiler which can of course run the compiled
code in the user's browser. That would be nice.

[^1]: Early binding allows for static verification of the existence of
      method-signature pairs (e.g. v-tables). Late binding does not give the
      compiler (or an IDE) enough information for existence verification, it
      has to be looked up at run-time.

[^2]: There are several hinting libraries, which developers insist are
      indispensable tools when developing JavaScript seriously, such
      as [JavaScript lint](http://www.javascriptlint.com/),
      [JSLint](http://www.jslint.com/), and
      [JSure](http://aurochs.fr/jsure.html).

[^3]: “Any non-trivial analysis is very difficult due to Javascript’s
      dynamic nature.” — Berke Durak, Ph.D., author of jsure.

[^4]: Google Inc. thought it necessary to develop [a compiler which
      does type-checking and limited
      inference](http://code.google.com/closure/compiler/), e.g.

        /**
         * Queries a Baz for items.
         * @param {number} groupNum Subgroup id to query.
         * @param {string|number|null} term An itemName,
         *     or itemId, or null to search everything.
         */
        goog.Baz.prototype.query = function(groupNum, term) {
          ...
        };

      This will ensure that invocations to `Bad.query()` will be
      well-typed. See the [Google closure
      docs](http://code.google.com/closure/compiler/docs/js-for-compiler.html)
      for more examples. Developers I've spoken to at Google say this
      makes JS bearable with sufficient self-discipline, but without
      it, maintaining a large codebase in JS is unrealistic.

[^5]: There are already many projects underway for doing this, such as
      [Pyjamas](http://pyjs.org/),
      [HotRuby](http://hotruby.yukoba.jp/),
      [Orto](http://ejohn.org/blog/running-java-in-javascript/),
      [ZK](http://www.zkoss.org/), and [many which merely provide a
      layer ontop of
      JavaScript](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS)
      to make using it marginally less painful. Some Haskell ones
      include
      [UHC](http://utrechthaskellcompiler.wordpress.com/2010/10/18/haskell-to-javascript-backend/),
      [YHC](http://www.haskell.org/haskellwiki/Yhc/Javascript),
      [jshaskell](http://code.google.com/p/jshaskell/) and
      [lambdascript](https://github.com/valderman/lambdascript).

[^6]: It is established that JavaScript is now a target platform due
      to its browser ubiquity. If we want to write tools,
      applications, games, etc. that will reach a wide audience with
      little effort on the user's part, targetting the browser and
      therefore JavaScript is an excellent option.

[^7]: Some ways that I see are:

      * Write JavaScript with a functional/OO/“better” standard
        library, a la
        [LambdaScript](https://github.com/runarorama/LambdaScript) or
        [CommonJS](http://www.commonjs.org/).
      * Write JavaScript with additional tools to provide static
        semantics (a la Google Closure).
      * Write JavaScript in an EDSL in another more powerful language,
        e.g. [ParenScript](http://common-lisp.net/project/parenscript/) and [HJScript](http://hackage.haskell.org/package/HJScript).
      * Create a new language that mostly preserves JavaScript semantics but
        adds some additional layer, e.g. [CoffeeScript](http://jashkenas.github.com/coffee-script/).
      * Interpret an existing language in JavaScript.
      * Compile an existing language to JavaScript.

      I prefer the last option.

[^8]: To put it in perspective, here are the extensions available in
      GHC as of 6.12.3: OverlappingInstances NoOverlappingInstances,
      IncoherentInstances, NoIncoherentInstances UndecidableInstances,
      NoUndecidableInstances, Arrows, NoArrows DisambiguateRecordFields,
      NoDisambiguateRecordFields ForeignFunctionInterface,
      NoForeignFunctionInterface, Generics NoGenerics, ImplicitParams,
      NoImplicitParams, NoImplicitPrelude ImplicitPrelude,
      NoMonomorphismRestriction, MonomorphismRrestriction NoNPlusKPatterns,
      NPlusKPatterns, NoMonoPatBinds, MonoPatBinds RelaxedPolyRec,
      NoRelaxedPolyRec, ExtendedDefaultRules NoExtendedDefaultRules,
      OverloadedStrings, NoOverloadedStrings, GADTs NoGADTs, TypeFamilies,
      NoTypeFamilies, ScopedTypeVariables NoScopedTypeVariables,
      MonoLocalBinds, NoMonoLocalBinds, TemplateHaskell NoTemplateHaskell,
      QuasiQuotes, NoQuasiQuotes, BangPatterns NoBangPatterns, CPP, NoCPP,
      PatternGuards, NoPatternGuards, ViewPatterns NoViewPatterns,
      UnicodeSyntax, NoUnicodeSyntax, MagicHash, NoMagicHash
      NewQualifiedOperators, NoNewQualifiedOperators, ExplicitForALl
      NoExplicitForAll, PolymorphicComponents, NoPolymorphicComponents
      Rank2Types, NoRank2Types, RankNTypes, NoRankNTypes, ImpredicativeTypes
      NoImpredicativeTypes, ExistentialQuantification
      NoExistentialQuantification, KindSignatures, NoKindSignatures
      EmptyDataDecls, NoEmptyDataDecls, ParallelListComp, NoParallelListComp
      TransformListComp, NoTransformListComp, UnliftedFFITypes
      NoUnliftedFFITypes, LiberalTypeSynonyms, NoLiberalTypeSynonyms
      TypeOperators, NoTypeOperators, DoRec, NoDoRec, RecursiveDo,
      NoRecursiveDo PArr, NoPArr, RecordWildCards, NoRecordWildCards,
      NamedFieldPuns NoNamedFieldPuns, DisambiguateRecordFields,
      NoDisambiguateRecordFields UnboxedTuples, NoUnboxedTuples,
      StandaloneDeriving, NoStandaloneDeriving DeriveDataTypeable,
      NoDeriveDataTypeable, GeneralizedNewtypeDeriving
      NoGeneralizedNewtypeDeriving, TypeSynonymInstances
      NoTypeSynonymInstances, FlexibleContexts, NoFlexibleContexts
      FlexibleInstances, NoFlexibleInstances, ConstrainedClassMethods
      NoConstrainedClassMethods, MultiParamTypeClasses
      NoMultiParamTypeClasses, FunctionalDependencies
      NoFunctionalDependencies, PackageImports, and NoPackageImports.

[^9]: On Ubuntu, I had to explicitly add -pthread to the build
      configuration of libraries/unix, otherwise it didn't figure it out
      automatically.

[^10]: There were maybe 5 `foo#` shaped functions that were out of
       scope throughout the base libraries, particularly in GHC. I simply
       replaced these with `undefined`, or because that's not available, `let
       a = a in a`, or whatever bottom value to stop it complaining. I don't
       know whether GHC will detect `let a = a in a`, I think it does. So the
       runtime will just throw an exception on these values.

           chris@cn-done:~$ cat > loop.hs
           main = putStrLn $ let r = r in r
           chris@cn-done:~$ ghc --make loop.hs -O2
           [1 of 1] Compiling Main             ( loop.hs, loop.o )
           Linking loop ...
           chris@cn-done:~$ ./loop
           loop: <<loop>>

       Looks OK.

[^11]: I called it `ghcjs_buildlibs`, and run it from the `ghc-6.12.3`
       directory. I think it should work fine for GHC 7, too.

         JSDIR=$1

         cd libraries/ghc-prim &&
         echo cding to libraries/ghc-prim &&
         ghcjs -odir $JSDIR/ghc-prim \
               -hidir $JSDIR/ghc-prim \
               -cpp -fglasgow-exts \
               -package-name ghc-prim \
               GHC/Types.hs &&
         ghcjs -odir $JSDIR/ghc-prim \
               -hidir $JSDIR/ghc-prim \
               -cpp -fglasgow-exts \
               -package-name ghc-prim \
               GHC/*  &&
         cd ../.. &&
         echo cding to ../.. &&
         cd libraries/integer-simple &&
         echo cding to libraries/integer-simple &&
         ghcjs -odir $JSDIR/integer-simple \
               -hidir $JSDIR/integer-simple \
               -cpp -fglasgow-exts \
               -package-name integer-simple \
               GHC/Integer.hs &&
         cd ../.. &&
         echo cding to ../.. &&
         cd libraries/base &&
         echo cding to libraries/base &&
         ghcjs -odir $JSDIR/base -hidir $JSDIR/base \
               -hide-package base \
               -package-name base \
               -I./include \
               -i./dist-install/build -XMagicHash \
               -XExistentialQuantification \
               -XRank2Types -XScopedTypeVariables \
               -XUnboxedTuples -XForeignFunctionInterface \
               -XUnliftedFFITypes -XDeriveDataTypeable \
               -XGeneralizedNewtypeDeriving -XFlexibleInstances \
               -XStandaloneDeriving -XPatternGuards \
               -XEmptyDataDecls -XNoImplicitPrelude -XCPP \
               Prelude.hs &&
         echo "Copying lib to main ..." &&
         cp $1/ghc-prim/GHC \
            $1/../main/ -R &&
         cp $1/integer-simple/GHC/ \
            $1/../main/ -R &&
         cp $1/base/* \
            $1/../main/ -R

[^12]: A [quick Google
search](http://www.google.com/search?aq=f&sourceid=chrome&ie=UTF-8&q=javascript+module+system)
       demonstrates easily enough that there is a need for a module system.

[^13]: Its support for closures is commonly noted as being one of
       JavaScript's redeeming features.

[^14]: I.e. a way to use closure callbacks for
       e.g. setInterval/setTimeout and AJAX, a way to serialize data
       structures like strings and arrays from/to Haskell and
       JavaScript, and a way to access the DOM and bind events to it.

[^15]: Pong is a good demo. I've already started work on this, but hit
       some walls when trying to separate the build into a more
       generic and less example-y structure. It's quite easy to break
       this system at present.
