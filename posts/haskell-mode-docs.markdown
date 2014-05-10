---
date: 2014-05-10
title: Haskell-mode documentation
description: Haskell-mode documentation
author: Chris Done
tags: haskell, emacs
---

## Background

So I've been using and adding to haskell-mode in Emacs for about 5-6
years now. All the while, I've pretty much documented none of it. Much
of all the other features that I use written by other people are also
not discoverable, either.

From the results of the
[haskell-mode survey](https://docs.google.com/forms/d/1FzWbzGm6odYWxJZcU3GFHlS3lVFTBOI1-M1c87CjOFg/viewanalytics),
it was clear that people wanted documentation, and that much of the
features we asked about, people weren't even aware existed.

Keeping with the spirit of Emacs, it's better to write a decent manual
and then let users customize and pick and choose as needed.

## Let's write some docs!

Herbert made a start on
[a texinfo manual](https://github.com/haskell/haskell-mode/blob/master/haskell-mode.texi),
which I thoroughly approved of. Until I started trying to add to
it. The texinfo format is a big and unfamiliar format. I got bored of
trying to figure it out and decided if *I* would be discouraged from
writing docs, so would anyone else who didn't even write the features.

On the other hand, the wikis on Github projects are written in
Markdown. Pretty much everyone knows Markdown. And you can work with
it as a Git repository. So I set to work down that route.

## Enter the haskell-mode manual

Here's about a day's worth of work:

[The haskell-mode manual](https://github.com/haskell/haskell-mode/wiki)

I've more things to add and flesh out, but this is pretty much all the
things I could think of from the top of my head. I'll keep adding to
it as and when I think of things. Hope it's useful!
