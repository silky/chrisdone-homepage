---
date: 2012-01-08
title: Moogle (movie reviews) and web frameworks
author: Chris Done
tags: haskell, movies, moogle, happstack, yesod, snap
---

My latest harebrained scheme, a movie/TV review web site that only
shows reviews by your friends and people you've chosen to see.

## Links of Trust

The problem with finding movies that you'll like is that critics and
mainstream reviewers are rarely aligned with your personal tastes.

But you can trust people you know to have similar tastes and to be a
good source of new  movies.

So you add people who you trust to give good reviews and subscribe to
notifications from them. You can also just browse their reviews to
find new movies.

## Review Everything

By giving a short review and some tags of every movie you watch, you
really help out your friends deciding what to watch.

* Step 1: Watch movie.
* Step 2: Jump onto moogle, save a little write-up.
* Step 3: Your friends get an email and check out the review, decide
  to grab the movie or decide they wouldn't like it.
* Step 4: You can come back later and see what your favourite movies
  are, or invite your friends.

## Design Mockup

I've done a design mockup pitching it as [moogle for reviewing movies
and TV](http://moogle.tv/). I want this site to look professional and
be simple and clean. I've produced the best I can do with Inkscape,
the rest CSS3 can take care of.

The idea of the site is quite trivial and implementation will be
easy. However, see the next section.

## The Framework

The next problem is choosing the web framework. The Big Three[^2] are:

* [Happstack.](http://happstack.com/)
* [Snap.](http://snapframework.com/)
* [Yesod.](http://www.yesodweb.com/)[^6]

Choosing between them is a very slow and boring task. They all appear
to be production-ready. I could just use FastCGI as I do at work with
my pre-existing web utilities that I wrote before these frameworks
existed, where I'd have everything I need to get going,[^1] but I feel
it's time to consolidate efforts.[^3]

Surprisingly I am unable to find a straight blog that compares all the
technical features and philosphies of Happstack, Snap and Yesod.[^4]
I'm not sure whether I have the time and motivation to make such a
long and detailed post, but I will try if so.[^5]

I might even use
[Ji](http://chrisdone.com/posts/2011-12-26-ji-haskell-web.html) or
[UHC JavaScript](http://chrisdone.com/posts/2012-01-06-uhc-javascript.html)
and abstract over it, we'll see.

More to come.

[^1]: Haskell is a powerful enough language that it's easy to work from the
      ground up.

[^2]: There's also Salvia which is also interesting and fairly
      complete, though I don't know if actively maintained or used
      (pretty sure the author uses Happstack).

[^3]: For example, I'd like to provide OpenID. This is implemented
      already in both Snap and Yesod (and probably Happstack). I don't
      feel like implementing and testing this myself. There is also
      the problem of contribution. Choosing a popular framework will
      aid contribution. It's just a shame I have to pick one.

[^4]: On the other hand one can find plenty of contention between the
      Yesod and Snap lead developers.

[^5]: It would seem that (few non-author) people have tried all three
      of these frameworks, otherwise there would be more comparative
      literature out there (suppose), this would imply that people
      pick the frameworks fairly arbitrarily, or based on the
      community.

[^6]: Not sure why they choose to stop using
      [my design](http://chrisdone.com/posts/2011-04-10-yesod-design.html),
      couldn't reworked the concept rather than scrapping it, but go
      figure.
