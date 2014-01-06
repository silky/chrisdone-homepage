---
date: 2014-01-01
title: Control of My Data
description: Control of My Data
author: Chris Done
tags: data, services
---

A new standard I was discovering for myself in 2012 was that maybe _I_
should be in complete control of data about me. In this case,
[the context was Last.fm](http://chrisdone.com/posts/lastfm). In 2013,
I applied that discontent to some real effort and pulled all my data
from Last.fm into a database that *I* control, presented on
[a simple web app](http://chrisdone.com/music) that *I* control.

The same way, it occurs to me that I should start importing from all
the services that I contribute data to regularly and for which the
data collected is actually useful in some sense. Cthulhu knows I have
the space for't. The ones off the top of my head are:

* GMail
* GoodReads
* Twitter
* Google+
* Reddit
* YouTube
* My bank web site

For GMail, I actually have no local copy of my email going back
to 2007. Six years worth of email is important data, for finding
references, for security and for interesting statistical analysis. In
fact, the reasons I'm still using GMail are few these days. I
regularly just cannot find the email I'm looking for. I don't like its
composing mode, at all. It's slow. In fact, I think the only reason I
use it these days is for the service of hosting my emails in a
reliable place.

GoodReads, like Last.fm, is a place where I've submitted things I like
in a regular format. The usefulness of that requires little
elaboration.

Twitter, Google+ and Reddit contain lots of references to materials,
and finding *anything* on these web sites is horrendous. They are not
made to be searched like an archive, they're made to only show the
latest stuff, understandably. Having a complete indexed archive of my
few years worth of these three web sites would be very handy. Not to
mention the possible statistical analysis.

My bank history should be very useful to analyze expenditure, profit,
bills, etc.

By comparison, Github is a service which archives. That's its one main
job. I can pull data from it at any time and generate data. However,
it's still worth pulling data from my activity feed.

Probably the rules should be:

* If the service provides no means of exporting, _do not use it_.
* If the service misrepresents your data and does not allow you
  to fix it, even if it exports, _do not use it_.
* If the service provides a means of exporting or feed importing, then
  do so, regularly.

My bank web site regrettably does not allow an automatic login
procedure, it requires a physical manual step. However, I can import
weekly with a manual step for login followed by automatic import.

Really, the main benefit to all these services, to me, is that they
are reliable (insofar as they still exist), they don't lose data, that
is all managed for you. I need to learn how to setup secure and
reliable replication of data like this that I want to endure over
time. I have my dedicated host, but that could malfunction at any
time. I should utilize dropbox (encrypted), my local storage and other
backup services.
