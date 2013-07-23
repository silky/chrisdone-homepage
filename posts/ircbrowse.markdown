---
date: 2013-07-23
title: IRC Browse
description: An update on things I've worked on and been up to lately
author: Chris Done
tags: haskell, life
---

Haven't blogged in a while, had some time to write now.

Since I last blogged, I made [IRC Browse.](http://ircbrowse.net/) It's
a service which allows you to browse the IRC logs of the #haskell and
#lisp channels of Freenode. The logs come from
[tunes](tunes.org/~nef/logs/), and, for
[Haskell](http://ircbrowse.net/browse/haskell), they go back to
2001. I like IRC. I don't go on it that frequently anymore, but I like
to read the logs and I see it for the useful communication and
coordination tool it is. I've always wanted a trivial way to view and
share IRC logs as a service, so I made one. The source code is
[here](https://github.com/chrisdone/ircbrowse).

It boasts these features:

* [A simple summary](http://ircbrowse.net/) of statistics on the home page
* [Browsing, page by page](http://ircbrowse.net/browse/haskell), all the logs
* [Searching](http://ircbrowse.net/browse/haskell?q=chrisdone) the logs
* [Viewing a statistics profile](http://ircbrowse.net/nick/chrisdone)
  of a particular person

It's written in Haskell, using Snap, PostgreSQL for the database, and
Sphinx for search. It's fast.

I made it ages ago, really, but thought it worth blogging about once.

## The IRC summary

The IRC summary is generated upon request, and reveals some possibly
interesting insights into channel activity and the top contributors.

Of interest the most is the activity by year, which indicates that
2009 was the apex of the IRC channel's activity, which has since
dwindled, and appears to be continuing to dwindle: despite sustained
activity, conversation generally is decreasing.

There are various hypotheses put forth for this. I speculate that

* People have been moving to other channels, such as #haskell-lens,
  #haskell-blah, etc.
* People are able to read reliable books that are now well publicized
  in contrast to in the past
* Some very active people have moved on

## Browsing

This is where the name “IRC Browse” comes from. There used to be a
service at ircbrowse.com, a few years back, providing a similar
browsing service. I asked the author of that old site whether I could
use the name ircbrowse.net, and they approved and wished me luck.

One thing that bugged me about the old IRC Browse was the speed. It
was god-awfully slow. It would take ages just to display one page of
logs. What I wanted was to have a log browsing service that would be
_instantaneous_ and snappy.

After some learning with PostgreSQL, I discovered some ways to make
paginating 26 million rows of a table quite fast. Simply using
`OFFSET`/`LIMIT` is far too slow—takes about one second to retrieve a
result. I couldn't simply query on the IDs, because there isn't just
one channel, or one pagination type. So I created a separate table to
store paging indexes. For every row of the "event" table, I created a
corresponding, ordered, row in the index table. After that, it was
snappy.

Another thing I discovered is that my pgsql-simple library was a
little sluggish. The pages would retrieve in, say, 50ms, rather than,
say, 2ms. So I switched the library to postgresql-simple and got the
extremely snappy responsiveness that I wanted.

## Searching

For searching I learned how to use the tool called Sphinx. It takes in
a configuration and a database, and then populates a search
index. From that search index, it provides very fast full text search.

I couldn't get the Sphinx library to work with the version of Sphinx I
was using at the time. I made a trivial wrapper to the command line
program instead. That worked. At some point I will replace this with
use of the Haskell sphinx library.

Another optimization I can do is split the indexes into #haskell and
#lisp.

## Profiles

Profiles give a nice way to tell when someone probably goes to sleep
and is probably available. It also tells whether someone has been
active lately. If they haven't been active lately, you can check their
complete history by year, and if you see it dwindling, perhaps they're
not on the IRC anymore.

There are also quotes @remember'd by lambdabot, which can be fun to
read.

## Importation

Importing the logs happens daily, at 10:30 UTC time. One day I might
update this so that it connects to the IRC directly and updates the
logs in real time. But I'm not sure it's worth it.

## Other stuff

I also did a social graph thing, but it's not that good and I will
probably remove it. There's a word cloud, which looks pretty enough,
I'll keep that.
