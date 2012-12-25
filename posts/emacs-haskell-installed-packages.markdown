---
date: 2010-08-07
title: Getting all installed Haskell packages/modules from Emacs
author: Chris Done
tags: haskell, emacs, elisp
---

I wrote a self-contained Emacs module for retrieving all the
installed Haskell package descriptions (as an association list),
and the exposed modules inside them.

* [Haskell installed packages](http://gist.github.com/511670)

This is intended for Elisp programmers to use to make import
handling and package handling easier.

For example, if I run `(haskell-installed-packages)`, I get:

    ("Cabal-1.8.0.2" "array-0.3.0.0" "base-3.0.3.2" "base-4.2.0.0"
    "bin-package-db-0.0.0.0" "bytestring-0.9.1.5"
    "containers-0.3.0.0" "directory-1.0.1.0" "extensi\
    ble-exceptions-0.1.1.1" "ffi-1.0" "filepath-1.1.0.3"
    "ghc-6.12.1" ...)

I can also run the following:

    (haskell-read-pkg-description
      (haskell-ghc-pkg-command "describe Cabal-1.8.0.2"))

And the following is returned:

    (("name" . "Cabal") ("version" . "1.8.0.2") ("id"
    . "Cabal-1.8.0.2-bc92fe595a99db06fca8c2eb712108b4")
    ("license" . "BSD3") ("copyright" . "2003-2006, Isaac Jones
    2005-2009, Duncan Coutts") ("maintainer"
    . "cabal-devel@haskell.org") ("stability:" . "") ("homepage"
    . "http://www.haskell.org/cabal/") ("package-url:" . "")
    ("description" . "The Haskell Common Architecture for
    Building Applications and Libraries: a framework defining a
    common interface for authors to more easily build their
    Haskell applications in a portable way. . The Haskell Cabal
    is part of a larger infrastructure for distributing,
    organizing, and cataloging Haskell libraries and tools.")
    ("category" . "Distribution") ("author" . "Isaac Jones
    <ijones@syntaxpolice.org> Duncan Coutts
    <duncan@haskell.org>") ...)

Finally I acquire a complete list of all modules installed with
`(haskell-installed-modules)`, which is defined in terms of the
above two functions.

    ("Distribution.Compiler" "Distribution.InstalledPackageInfo"
    "Distribution.License" "Distribution.Make"
    "Distribution.ModuleName" "Distribution.Package"
    "Distribution.PackageDescription"
    "Distribution.PackageDescription.Configuration"
    "Distribution.PackageDescription.Parse"
    "Distribution.PackageDescription.Check" "Distribution.ParseUtils"
    "Distribution.ReadE" ...)

These functions cache automatically. You can force them to
refresh the cache by passing `t` to them. Or run the following
function.

In your .emacs start-up file, put:

    (haskell-installed-packages-refresh-all)

Which will refresh all packages and modules. It takes about 3
seconds on my machine. But I am okay with that because I restart
Emacs rarely. You do not have to put it here, but it saves
waiting for it to update when you are mid-way through some
action.

In the future it is completely possible to speed this up by
checking the last modified date on the Haskell package
databases, and to only update the changed packages.

For my use case, I am going to use it for smex or
anything.el-like completion and write another module with clever
import insertion so that one can navigate between import
statement parts with arrow keys. What can I say?; I really do not
like typing out import statements.

For Emacs newbies, if you are impatient and want to use this
right away, pop this in your .emacs file:

    (require 'haskell-installed-packages)
    (setq haskell-ghc-pkg-bin-path "/home/chris/Programs/bin/ghc-pkg")
    (haskell-installed-packages-refresh-all)

You can omit the `setq` line if it works without it; otherwise
change it to wherever your ghc-pkg binary lies. Now a very simple
shortcut for completing Haskell modules:

    (defun haskell-complete-module ()
      "Insert a completed Haskell module."
      (interactive)
      (insert (ido-completing-read "Module: "
                                   (haskell-installed-modules))))

Then bind it to a key:

    (define-key haskell-mode-map (kbd "C-c i") 'haskell-complete-module)

Or whatever you prefer.
