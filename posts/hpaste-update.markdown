---
date: 2010-09-15
title: Amelie: hpaste.org gets an update
description: Amelie: hpaste.org gets an update
author: Chris Done
tags: blog, haskell, hpaste
---

The hpaste.org site had some database locking issues and the
person hosting it has not been around to fix it for a long
time. I thought the source code for the site was a bit big, so I
thought it would be a neat project to write from scratch as
[a long screencast about writing a Haskell project from scratch,
using Hackage libraries, etc..](http://github.com/chrisdone/amelie-emacs-cast)
I will post more about the screencast later once I am done. The project is
called [amelie.](http://github.com/chrisdone/amelie)

[Bryan O’Sullivan](http://www.serpentine.com/blog/) kindly
pointed the [hpaste.org](http://hpaste.org/) domain to my new
server. The service is also available under the domain
[paste.tryhaskell.org](http://paste.tryhaskell.org/).

I plan on adding some niceties like
[hlint](http://community.haskell.org/~ndm/hlint/) and the
[Context in IRC](http://bc.tech.coop/blog/041020.html) feature
that paste.lisp.org used to support. I will add some trivial spam
filters depending on the type and frequency of the spam
recieved. I will also add an API pretty soon and an RSS
feed. Once the spam's sorted out we can properly bring back the
hpaste bot into the IRC channel.

I also have an archive of all the old pastes from hpaste.org
(there are 37,000 of them). I have already written an import script
for this so I will get those imported sometime this week.

I recently had to switch my VPS host to a new one, it may be that
tryhaskell will be a little twitchy. It seems okay though!
Contact me at [chrisdone@gmail.com](mailto:chrisdone@gmail.com)
if you experience problems.

(Yes, I did just re-use the design/colours from TryHaskell.)
