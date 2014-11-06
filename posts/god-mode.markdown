---
date: 2013-09-21
title: God-mode for Emacs
description: Evaluation of God-mode for Emacs
author: Chris Done
tags: emacs, vim
---

A month ago I blogged about ways to reduce strenuous key presses in my
Emacs use. I
[analyzed my runs of chords in Emacs](/posts/emacs-key-analysis), then
[speculated](/posts/speculations-on-exclusive-editing) on the merits
of
[exclusive vs mixed editing](http://chrisdone.com/posts/modal-editors). Since
then I wrote an Emacs mode called
[god-mode](https://github.com/chrisdone/god-mode/). It's a mode that
you toggle in and out of, and when you're in it, all keys are
implicitly prefixed with `C-` (among other helpful shortcuts). Over
all, it's been a resounding success. A couple other people, including
the author of multiple mark mode, contributed some patches. I've been
using it for a month and have been very satisfied.

For those interested in the keymapping as of writing, you can [skip to
that section below.](#the-keymapping)

## Compared with Vim

Coming back to the examples I came up with in [speculations on
exclusive editing](/posts/speculations-on-exclusive-editing), God-mode
is on par with Vim. In fact, it's the same or fewer key presses for each one:

    Vim:   bbDi (5)
    Emacs: gb.ki (5)

    Vim:   db.i (4)
    Emacs: g←.i (4)

    Vim:   ?r↲lDi (8)
    Emacs: rr↲ki (5)

That's not bad. I grant that my Vim fu is weak, so probably there are
shorter ways to write the Vim examples. But at any rate Emacs is doing
well here.

## Evaluation After One Month

I've been using this in my Emacs turned on by default for one month. I
knew I was going to stick with it after a week or so of use, it was
already ingrained into how I use Emacs. Now, when I access a remote
Emacs on a server or whatnot, I find that I reach for the Caps Lock
key (my toggler key) in order to do an involved editing operation,
only to find that it's not there! Oh, no! I'll have to use Ctrl for
all these dull commands…

In typical usage, it's a 50-50. When I'm writing code, I tend to
work in normal Emacs mode. When I'm editing, I work almost exclusively
in God-mode. Cutting (`w`), copying (`gw`), moving (`k`, `gk`, `Gk`,
`y`), navigating (`ga`, `gf`, etc.), reindenting, slurping, buffer
switching (`z`), running commands (`cc`, `ci`, `ct`, etc), moving
around (`e`, `a`, `f`, `b`, etc.), searching (`s`, `r`), replacing
(`t`), saving (`xs`). All those things I do from god mode.

I've also noticed that the more tired I get with my hands towards the
end of the day, the more I tend to stick in god-mode. That gives me
extra mileage to finish those last things.

## Retaining God Mode Exclusively

In fact in some modes it's possible to remain entirely in God mode. In
CSS mode, for example, I'm able to produce out the following:

    .foo {
       display: none;
    }

by typing

    { .foo ↲ : d ↲ ↲

What happens there is that `{` prompts me for a rule and inserts `{ }`
and puts my cursor inside it. Then `:` prompts for a property name,
which is completed with ido-mode. Then it prompts for a value. In the
case of the `display` property, it knows there's only a list of values
available for it, and it prompts for a choice of `none`, `block`,
etc. I hit `↲` to choose the default.

If I want to edit a property/value pair, I hit `;` and it prompts me
for the value with the input containing the existing value.

The more one is able to stay in God mode, the more the speed and
convenience benefits.

## The Keymapping

(This is described in the README, but including here for posterity.)

God-mode defines the following mapping:

* All commands are assumed to be `C-<something>` unless otherwise
   indicated. Examples:

   * `a`    → `C-a`
   * `s`    → `C-s`
   * `akny` → `C-a C-k C-n C-y`
   * `xs`   → `C-x C-s`
   * `x s`  → `C-x s`

   Note the use of space to produce `C-x s`.

* `g` is a special key to indicate `M-<something>`. This means that
   there is no way to write `C-g` in this mode, you must therefore
   type `C-g` directly. Examples:

   * `gf` → `M-f`
   * `gx` → `M-x`

* `G` is a special key to indicate `C-M-<something>`. Example:

   * `Gx` → `C-M-x`

* Digit arguments:

  * `12f` → `M-12 C-f`

* Repetition:

  * `gfzz` → `M-f M-f M-f`

* Universal boolean argument:

  * `uco` → `C-u C-c C-o`

* There is a key (default `i` - think *insert*) to disable God mode,
  similar to Vim's i.
