---
date: 2013-08-08
title: Speculations on exclusive vs mixed editing
description: Speculations on exclusive vs mixed editing
author: Chris Done
tags: emacs
---

I covered what exclusive vs mixed editing means in
[another page](/posts/modal-editors).

I think it depends on what you're doing. If you're writing stuff
e.g. in insertion mode, the mode switching in vim becomes a pain
because every time you want a command you have to switch to normal
mode and then switch back again. E.g. consider the following (where
`|` indicates the cursor):

    Lorem ipsum dolor sit amet, consectetur adipiscing elit|

If I'm writing this sentence and I realise the last two words are bad
and should be another word, in Vim I might run the following to go
backwards two words and delete to the end of the line:

`C-c b b D i` — 7 key presses to do the operation

In Emacs I would run

`M-b-b C-k` — 5 key presses

Or in Vim I might run the following to just delete the last two words:

`C-c d b . i` — 6 key presses

which in Emacs is

`M-<backspace>-<backspace>` — 3 key presses

Or I might instead search to the “r” character and then delete from there:

`C-c ? r RET l D i` — 10 key presses

which in Emacs is

`C-r r RET C-f-k` — 7 key presses

It's just an example and the gains or losses will vary depending on
the example. But Emacs's default keybindings optimize the writing case
where you are inserting and editing with equal fervor, whereas Vim's
mutal exclusion optimizes the editing mode, and I'm told leaves a lot
to be desired for insertion mode.

What I've been considering, and was [the point of my other
page](/posts/emacs-key-analysis), is to gather some real data on how
often I am doing these C-/M- commands and for how much. Looking at the
data, it turns out that I only do about 2.3 unique commands at one
time, so `C-c b b i` in Vim is not a gain over M-b-b.

On the other hand there are some commands that I repeat a lot
(probably `C-f`/`C-b`/`C-n`/`C-p` for navigation that I can probably
combat by making repetition slow, encouraging me to use search more),
and some editing commands that are too costly to type but also common,
and so ought to be rebound. It's not yet clear that mutually exclusive
editing is a big win in the general case, but I am quite serious about
trying it for editing Haskell and Lisp code, where there are slightly
longer runs of commands.
