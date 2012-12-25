---
date: 2010-04-11
title: Yay! hpaste bot is back
description: Yay! hpaste bot is back
author: Chris Done
tags: haskell, hpaste
---

The [hpaste](http://hpaste.org/) site used to join the #haskell IRC
channel and sit there waiting for pastes. When someone made a paste it
would announce them in the channel with a link. Half a year ago (or
longer, I guess) hpaste was rewritten and hpaste2 was
born. Unfortunately, the IRC announcer failed to work on the server it
runs on.

It’s a pretty convenient feature and it encourages people to paste and
share the code they’re having problems with and the code they find
interesting and want to share. If hpaste sends the link for you, it’s
not an active thing on your part.

I figured it would be a good idea to use the [rss2irc program on
Hackage](http://hackage.haskell.org/package/rss2irc), which, I
believe, is what the Hackage IRC announcer uses. E.g.

    * hackagebot hashed-storage 0.4.11 - Hashed file storage support code.
      http://hackage.haskell.org/package/hashed-storage-0.4.11 (EricKow)

I read the hpaste source and it has some code for generating an RSS
feed but it’s all commented out. So I wrote [a little program to
periodically fetch the main page of hpaste and generate an RSS feed
from it](http://github.com/chrisdone/hpaste-feed) and write it to [a
file in my web server directory.](http://tryhaskell.org/hpaste.rss)
I’ve been thinking of doing this for more than half a year, but thought
"someone’ll sort it out".. never did. So here we go.

I run it like this:

    hpaste-feed /var/www/tryhaskell.org/hpaste.rss 30

And then just hook-up rss2irc with it:

    rss2irc http://tryhaskell.org/hpaste.rss \
    'hpaste@irc.freenode.net/#hpaste-test' \
    -i 1 -l --author & disown

I make a paste, and within a couple seconds, tada!

    <hpaste>  a test paste (chrisdone) http://hpaste.org/fastcgi/hpaste.fcgi/view?id=24822

I’m running it on the tryhaskell server so hopefully it should be
stable 24/7. Only problem is hpaste.org going down, but can’t be
avoided. I’ll need to see what errors are thrown with the
[Network.HTTP](http://hackage.haskell.org/package/HTTP) library when
the request times out or fails.
