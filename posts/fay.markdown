---
date: 2012-09-15
title: Fay, JavaScript, etc.
description: Fay, JavaScript, etc.
author: Chris Done
tags: haskell, fay, javascript
---

A couple months back I released Fay in a preliminary stage, with a
[little web site of its own](http://fay-lang.org/). I haven't blogged
about it yet, so I thought I'd do that.

## Setting the scene

And lo, when God created the world,
he looked at it,
and saw that it was good.

When Brendan Eich created JavaScript,
he looked at it,
and saw that it was good enough given the questionable requirements
and strict time constraints.

When I look at JavaScript, I see that it is bad. And not good enough
given the various other superior languages out there.

But I repeat myself, see [The JavaScript
Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem)
for more details.

## Recognizing it as a real problem

I think any developer with their head screwed on knows about the above
problems and that JavaScript needs to be replaced as soon as
possible. But the problem is immediate.

My approach to the problem, as with everyone else, has long been:
well, we can't do anything about it, let's just wait for Google to
complete their native client project and hope that it breaks the
market. Or, let's wait until the existing compiling-to-JavaScript
solutions become usable.

Any way you look at it, as you sit down to write a new project, and
every time you get a stupid error due to JavaScript's wackiness, you
say to yourself “just one more project in JavaScript… just this quick
script…”

After seeing Inventing on Principle[^1], I was profoundly influenced
by Bret Victor's message. His talk was impressive, but his message was
moreso. The idea that I took away from watching it was:

“If you recognise something as a problem, and you have the capability
to fix it, you have a moral duty to fix that problem.”

I'm not sure I have such strong convictions as Bret to apply that
generally, but his principled approach influenced me. One day I wanted
to write a web app, and got that sinking feeling of wasting it on
JavaScript, and decided never to write any JavaScript again for a new
project.

## Fixing the problem

I decided that to make such a claim, I should have to back it up with
a solution, and do it fast. So I spent that weekend hacking on a
Haskell compiler for JavaScript. I spent another weekend polishing it,
and on the third week I was using it at work in production. Back then
the project was called “hj.” And for months it sat hidden, private to
me. A mini-success and a solution to the problem I saw.

I'll note that Elm and Roy also inspired me to give it a go.

## Reaction

Fast-forward a couple months, I decide it's time to re-brand it to
something friendly and put it online. I called it “Fay.”

Someone posted it to Reddit's programming forum and Hacker News, and
the site got about ten thousand hits in two days. Lots of interest
generated, and people emailed me asking what the implications of such
a project are. That's really encouraging!

I got invited to talk at [LXJS](http://2012.lxjs.org/), a JavaScript
conference. I will be going in two weeks. Ironically, I will go to
basically say how much I dislike JavaScript to a crowd of people who
mostly like JavaScript, but that's how I roll.

After putting it on Github I had quite [a few
contributions](https://github.com/faylang/fay/network), too!

Today and tomorrow I'll be producing a bunch of demo examples of Fay
code, and finalizing my short 20-minute talk.

## Future work

Fay is missing some things that would be nice to have:

* Type-classes
* Tail-call optimization
* Strictness analysis
* Source mappings
* Cabal support
* Lots more other stuff

But these can wait a bit of time. I could/would write more, in more detail,
but I have a lot of stuff to do at the moment. So, apologies for the
brief post, but I thought it was worth having this post in the blog
for the sake of chronology, and to get this series of events out of my
system. That's the point of a blog, right?

[^1]: Watch it on [Vimeo](http://vimeo.com/36579366), or on
      [YouTube](http://www.youtube.com/watch?v=PUv66718DII).
