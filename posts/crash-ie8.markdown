---
date: 2010-09-30
title: Crashing IE8 with two lines of code
description: Crashing IE8 with two lines of code
author: Chris Done
tags: internet explorer, despair, microsoft
---

In my wonderful time developing for IE8, I discovered, after some
swearing, this gem:

    <script src="http://code.jquery.com/jquery-1.4.2.min.js"></script>
    <script>$('<link rel="stylesheet"/>').attr('href','');</script>

Are you running IE8? [Crash my browser.](http://chrisdone.com/code/ie8-crash.html)

I can't believe in 2010 I can write some JavaScript that can **crash** a
web browser. But, then, taking *all* things into consideration, I can.

I downloaded the latest IE8 (8.0.6001.18702) and am running this
in XP SP2 in VirtualBox. I hope, but doubt, this bug has been
fixed, and that it does not crash your browser. Kind of.
