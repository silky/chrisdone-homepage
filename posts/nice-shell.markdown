---
date: 2012-02-07
title: Towards a nicer shell?
description: Towards a nicer shell?
author: Chris Done
tags: ideas, haskell, shells
---

Just an idea to look into when I have more time.

It would be kinda cool to write a shell that had intimate knowledge of
the options and input/output types of shell commands, by
user-contributed interfaces.

So, “oh, I just installed notify-send, I'll look online for a
notify-send wrapper”, install, and poof, I have option completion and
validation, including completion for those enumerated types like
LEVEL, ICON, NAME that are *sometimes* in the man page.

Could include easily-accessible useful examples and such, especially
for those obscure commands with non-obvious interfaces.

A Haskell shell with user-contributed wrappers is one reasonable
route. There are
[already Haskell shells out there](http://www.haskell.org/haskellwiki/Applications_and_libraries/Operating_system#Haskell_shell_examples),
but most are dead. There is room here to replace the regular shell in
the same way XMonad replaced the regular window manager.
