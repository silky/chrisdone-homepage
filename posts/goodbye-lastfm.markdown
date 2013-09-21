---
date: 2013-09-15
title: Goodbye, Last.fm
description: Last.fm / scrobbling
author: Chris Done
tags: last.fm, music, scrobbling, haskell
---

*tl;dr: I replaced my dependence on Last.fm with my own service
 [http://chrisdone.com/music](http://chrisdone.com/music).*

In my [last post](/posts/lastfm) (admittedly, a year ago) I talked
about how Last.fm had served me well enough over the years, but that
I'd grown tired of not having full access to my data in order to make
corrections. The final straw yesterday was that I have two Last.fm
profiles; one from years back, and one for the past few years. In
total I have 6 years worth of music listening information that I
wanted to merge into one. Last.fm said no, you cannot do this. For a
data aggregation service, that's bonkers. Sod it.

I used
[lastexport.py](https://github.com/encukou/lastscrape-gui/blob/master/lastexport.py)
which was written by the [libre.fm](http://libre.fm/) guys. I
downloaded everything from both of my profiles into an 11MB CSV
(tab-separated) file, I also downloaded my loved tracks. This gave me
the only information I cared about from Last.fm. I don't need Last.fm
anymore.

I tried to use the
[cassava](http://hackage.haskell.org/package/cassava) library to parse
the CSV but it failed to parse and threw ridiculous 11MB error
messages. I tried out
[csv-conduit](http://hackage.haskell.org/package/csv-conduit) and it
parsed it first time. The APIs are similar, so I didn't care which one
worked. I imported my data into a PostgreSQL database. As of writing
there are 83,238 listens recorded.

I then wrote a trivial web server with the
[scotty library](http://hackage.haskell.org/package/scotty). It's not
that much more trivial than
[snap](http://hackage.haskell.org/package/snap), but it has a low
dependency foot-print and doesn't require doing ByteString conversion
to get anything done. I made a simple handler to accept track
submissions that would record into the database.

I made a simple greasemonkey script for
[Grooveshark](http://grooveshark.com) (which is pretty much all I use
right now to play music) that would submit tracks every 30 seconds to
my local server. I'll probably make a script for YouTube, too, because
I sometimes listen to music on there. Grooveshark supports
“favouriting” tracks, which I also added a hook for—when a track is
favourited, it submits it to my server as “loved”. Like Last.fm, I
don't care about what Grooveshark stores.

I added to the web server a little summary page, then I installed it
on my Hetzner dedicated host under chrisdone.com,
[here](http://chrisdone.com/music).

I merged “The Prodigy” and “Prodigy” into one artist, because
“Prodigy” is some rapper that I don't like nor have ever listened
to. This brought The Prodigy up to my most listened-to artist, which
is accurate.

Job done, back to listening to music and hacking on important
things. Goodbye, Last.fm.
