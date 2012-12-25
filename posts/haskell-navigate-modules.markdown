---
date: 2010-08-06
title: Navigating to/from Haskell import lists in Emacs
description: Navigating to/from Haskell import lists in Emacs
author: Chris Done
tags: haskell, emacs, elisp
---

I wrote a self-contained Emacs module for navigating to and from
the import list in Haskell files.

* [Haskell modules navigator](http://gist.github.com/511240)

The use case is, for example, two sets of module lists:

    import           Control.Applicative                   ((<$>))
    import           Data.Bool.Higher                      (bool)
    import           Data.List                             (sort,intercalate)
    import           Data.Maybe                            (catMaybes)
    import           System.Process                        (readProcess)
    import           System.Environment                    (getArgs)

    import qualified Distribution.PackageDescription.Parse as P
    import qualified Distribution.PackageDescription       as PD
    import qualified Distribution.ModuleName               as PM
    import           Text.Regex.Posix                      ((=~))

And I am half way down the file, and realise I need to import
`Control.Concurrent`. So I hit `F8`, which takes the cursor to the
`Control.Applicative` line. I enter my import statement, and then
hit `C-u F8` to return back to the middle of the file to what I was
doing.
