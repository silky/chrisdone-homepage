---
date: 2012-06-11
title: Last.fm / scrobbling
author: Chris Done
tags: last.fm, music, scrobbling, haskell
---

Last.fm has served me well over the years. I don't remember it ever
being down. They went from free to paying for listening, but at a
couple euros per month, I didn't mind, at all. I'm a subscriber now. I
didn't mind paying for Spotify either.

But what bugs me about the services; Last.fm, Spotify, and
Grooveshark, etc. is that the information is not mine to control. I
can't fix it, there's no crowd-sourcing. Whether the data is about me,
or about an artist, I can't change it. If I'm offline, I can't get at
it.

In Spotify I asked them to correct the track titling of an album I
liked to listen to as half of the track titles were in the wrong
order, a year later it remains the same. Fine. BE LIKE THAT!

Since for as long as I can remember, Last.fm has confused various
artists together, but notably Prodigy and The Prodigy. The Prodigy are
an electronic/dance/techno outfit that I love, and Prodigy is a rapper
I've never heard of. They haven't solved this despite the five year
old complaints about it. Fine. I ain't even mad.

Grooveshark's meta data is a joke, not worth criticizing.

Such discrepancies aren't a huge deal, but they are a problem. And
they make me realize more and more how much I value having control of
my data. As a normal everyday user, I can only be satisfied with what
I'm given, maybe I can complain, but it's likely to be ignored.

But as a programmer, someone who has the know-how to scrape the data
and import it into a database and create an alternative
infrastructure, I almost have a moral duty to myself to fix it! It's a
principle!

So the first thing I did was use [a
program](http://encukou.github.com/lastscrape-gui/) which scrapes from
the public-access API about 50 tracks per second. I had 70k tracks to
download, so this took some time, and it failed the first time, I had
to patch the source to resume and then merge the output later.

Then I wrote a little Haskell script to rewrite that 8MB file into an
SQL script, and imported into PostgreSQL. Lovely. I can make all sorts
of charts and graphs and I can mash-up data from MusicBrainz, buuuut,
wait…

Now, it's all well and good with kendle mint cake having the past
data, but what about new tracks? New scrobbles? Now I need some way to
record new scrobbles into my personal database. I googled around
looking for some simple scrobbling services but didn't turn anything
up. I had a look at the scrobble protocol, and it's actually really
simple. So I made [this.](https://github.com/chrisdone/scrobble) A
library to purely handle taking “currently listening” notifications,
and scrobbles.

Now all that remains is a few lines to insert into the database on
scrobble. Another thing that would be nice is to also push the same
data to Last.fm and Libre.fm, just to retain that social aspect (user
compatibility) that those web sites provide.

But I am enticed at the prospect of extending the notion of an “open
standard” — if scrobbling is an open standard, why not music profiles?
Why tie ourselves down to one implementation?
