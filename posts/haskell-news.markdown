---
date: 2013-07-26
title: Haskell News
description: Haskell News post
author: Chris Done
tags: haskell
---

As a consumer of Haskell content I neither have the time nor inclination to
follow haskell-cafe and other various mailing lists, the reddits, the
google+ community, planet haskell, hackage releases, twitter, youtube and
whatever other submission places I haven't heard of.

I wrote this [to
Haskell-Cafe](http://www.haskell.org/pipermail/haskell-cafe/2013-February/104898.html)
four months ago. Only one person responded, which was [Daniel Díaz
Casanueva](http://www.haskell.org/pipermail/haskell-cafe/2013-February/104899.html),
the producer of [Haskell Weekly
News](http://contemplatecode.blogspot.it/). Naturally this would be a
great resource for him, as it would summarize everything of the past
few days in one place.

Shortly after this post I went ahead and implemented [Haskell
News](http://haskellnews.org/) for which you can find [the source code
here on GitHub](https://github.com/chrisdone/haskellnews/).

It has two views: grouped and mixed. Grouped lists all the items
according to their source, and mixed lists everything in a flat
stream. The mixed view polls for updates every ten minutes, so that
users can leave it open in a tab a la Twitter. There is also
[an RSS feed](http://haskellnews.org/feed), because I heard you like
feeds, so I put a feed in your feed so you can subscribe while you
subscribe.

The sources, as of writing, are:

* Reddit: /r/haskell and /r/programming posts containing the word “haskell”
* Haskell-Cafe
* Stack Overflow: questions tagged “haskell”
* Pastes: pastes made to the #haskell channel
* GitHub: updates for projects that contain Haskell code
* Planet Haskell
* Google+: posts to the Haskell community
* Twitter: posts tagged "#haskell" or containing "Haskell"
* Hackage
* IRC Quotes: things that have been @remember'd in the #haskell channel
* Vimeo: various Haskell related feeds
* HaskellWiki: updates to the wiki

I think this paints a fairly comprehensive picture of the Haskell
community's public activities. Certainly, if you want Haskell news,
here is the best place online to go.

All the feeds are updated every ten minutes. All of the feeds are
taken from RSS or Atom feeds, with the exception of three, which I
scraped with tagsoup:

* Google+, which provides no RSS feed (but they do provide an API,
  which I could look into if I had nothing better to do)
* Twitter, which no longer provides an RSS feed (but they do provide an API,
  which I could look into if I had nothing better to do)
* Github, which does not provide an RSS feed for language-specific
  project updates (I don't know if they have an API, nor care too much)

All feed items are stored in a database forever. There are currently
17k entries from 4 months of running. Feeds are unparseable for the
feed library from time to time.
