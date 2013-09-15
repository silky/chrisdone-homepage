---
date: 2013-08-07
title: Analysis of Emacs keys pressed
description: Analysis of Emacs keys pressed
author: Chris Done
tags: emacs
---

Here's the deal: Emacs keybindings make my fingers hurt. I don't think
I ever experienced RSI before I started using Emacs. I guess I've been
using Emacs for about 6 years. I'm very efficient with it. I can edit
almost as fast as I can think, my fingers never need to take a
break. But that efficiency comes at a steep price, I feel.

I hypothesize that chords are to blame, and that I would be happier
and less achey if I used a modal set of keybindings, like in Vim, in
which every key binding is a single character. Not all the keybindings
(e.g. `$`) are a single key press, but most are.

I've tried `evil-mode`, and it's pretty poor. It doesn't provide a
proper mapping to Emacs; hitting `$` doesn't actually execute
`move-end-of-line`, it executes `evil-end-of-line`, which does not
integrate with existing modes well at all. It's catering to Vimers,
but it's not good for Emacs power users.

I suspect that I would like to have a global modal switcher that will
make `C-` and `M-` implicit somehow, so that `a SPC e w` is equivalent
to typing `C-a C-SPC C-e C-w`. Before sitting down to develop such a
system, tackle the problem of how to start and exit the mode, and how
to deal with the meta key, I thought I would collect some
statistics. (And actually there are systems like sticky keys or chords
for Emacs for tackling stuff like this, so it's not a scary, new
area.)

What I wanted to prove (or collect evidence for) was:

* I waste a lot of energy on `C-` and `M-` commands.
* Said commands happen in clusters, which would justify a modal
  switcher.

I already had [a trivial script](http://lpaste.net/91637) to print key
presses for screencasts, so I modified that to also store the time and
mode in the buffer, and I opened a `keys.log` file to which I would
save the key presses for a day.

I then whipped up a [script](http://lpaste.net/91638) to read in those
statistics and print out a summary, to (hopefully) provide evidence
for the above claims.

The output is the following:

> Recording start/end: 2013-08-07 09:52:23 UTC/2013-08-08 07:54:23 UTC
>
> Time spent: 22 hours (not 100% activity)
>
> Total key presses: 29687
>
> Commands (including character presses): 22657
>
> Single-key commands: 16457
>
> C- or M- commands: 6200 (27.36%)
>
> Runs of (consecutively) unique C-/M- clusters: min/max/avg/stddev:
> 1/45/2.25/2.52
>
> Runs of non-unique C-/M- clusters: min/max/avg/stddev:
> 1/189/3.35/7.04
>
> Key presses used on C-/M- commands: 13230 (44.56%)
>
> Runs of C-f/C-n/C-p/C-b: min/max/avg/stddev: 1/39/2.96/4.63
>
> Key presses used on C-f/C-b/C-n/C-p: 4572 (15.40% of all key
> presses, 34.56% of C-/M- command key presses)

    Top commands used:

     1 |   750 | C-n
     2 |   716 | C-p
     3 |   355 | C-f
     4 |   341 | C-/
     5 |   335 | C-b
     6 |   259 | M-DEL
     7 |   248 | C-z
     8 |   245 | M-b
     9 |   231 | C-e
    10 |   221 | M-p
    11 |   193 | C-d
    12 |   189 | M-f
    13 |   157 | C-s
    14 |   145 | C-M-u
    15 |   142 | C-g
    16 |   136 | C-a
    17 |   117 | C-y
    18 |   109 | C-x C-s
    19 |   107 | M-
    20 |    92 | C-SPC


Terms:

* _Unique_: `C-SPC C-a C-w` -- this would cut the text from the point
  to the start of the line, that's a unique cluster.
* _Non-unique_: `C-f C-f C-f` -- move forward three times, that's a
  non-unique cluster.

For unique clusters, I'm doing 2.26 commands per cluster. So if I used
sticky keys, or a modal switcher, it would not be a gain. E.g. `C f f
C` vs `C-f C-f` is no gain, it's actually more presses due to having
to hit C again.

But in terms of non-unique clusters, there's a gain at 3.44 commands
per cluster. That means `C f f f C` vs `C-f C-f C-f`, which is one key
less pressed. If I'm pressing `9218` keys for `C-`/`M-` commands,
there might be a 20% decrease in key presses.

I'd love to see a similar analysis done of Vim. How often do Vim users
switch from insert mode to normal or presentation mode? I will
continue recording my keys for the next couple of days.

Very interesting is how much I use navigation functions. In reaction
to this, I'm disabling those keybindings and switching to arrow
keys. And I've found `M-e`, a more convenient binding for `C-M-u`. I
will also stop using `C-d` and use `DEL`.

[Follow-up page →](/posts/speculations-on-exclusive-editing)
