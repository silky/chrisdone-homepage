---
date: 2013-08-08
title: Modal Editing: exclusive vs mixed
description: What modal editing is
author: Chris Done
tags: emacs
---

First I should clarify what I mean by exclusive vs mixed editing:

* _Exclusive_: In Vim you can only insert or edit, those are two different modes.
* _Mixed_: In Emacs and most other editors, insertion and editing are
  done without switching modes.

Now we can say what a mode is. Modal editing means that the effects of
your key presses depend on the mode currently activated. From that we
can say that both Emacs and Vim are modal editors:

Vim only has three modes as far as I know:

* Insertion mode
* “Normal” mode (editing)
* Last line mode

Emacs has hundreds of modes:

  * Mini-buffer mode
  * Text mode
  * Lisp mode
  * Dired mode
  * Magit mode
  * Shell mode
  * etc.

In light of this, calling Vim “modal” as if it's the only editor that
does this confuses matters. The real difference is the exclusion.