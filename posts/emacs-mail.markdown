---
date: 2014-02-08
title: Emacs, Notmuch and Offlineimap
description: Emacs, Notmuch and Offlineimap
author: Chris Done
tags: emacs, mail
---

I kind of hate writing in anything other than Emacs. Especially
email. Writing email in Emacs with `message-mode` is lovely. I get all
my editing facilities and any key bindings that I want. More than that,
generally managing my mail in Emacs rather than relying on what's
available in the GMail UI is desirable.

So I moved my email reading to Emacs. Here's how I did it.

# Offlineimap

First, I installed offlineimap. Second, I made a `~/.offlineimaprc` configuration file:

``` python
[general]
accounts = ChrisGmail

[Account ChrisGmail]
localrepository = Local
remoterepository = ChrisDoneGmail

[Repository Local]
type = Maildir
localfolders = ~/Mail

[Repository ChrisDoneGmail]
type = Gmail
maxconnections=1
remoteuser = chrisdone@gmail.com
realdelete=no
folderfilter =
  lambda foldername:
    foldername in ['[Google Mail]/All Mail',
                   '[Google Mail]/Sent Mail']
nametrans =
  lambda foldername:
    re.sub('^\[Google Mail\]/All Mail$', 'all',
       re.sub('^\[Google Mail\]/Sent Mail$', 'sent',
         foldername))
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
remotepass = <my password here>
```

*Note: if you're copy-pasting this file, you will need to put the
`folderfilter` and `nametrans` on one line. Python doesn't like
multi-line lambdas.*

I ran

    $ offlineimap

I let that run for 10 hours to download all 19K messages from my 8~
year old GMail account.

# Notmuch

To index, tag and search that mail, I installed Notmuch. I configured
it using `notmuch config` and followed the guide.

My current configuration (`~/.notmuch-config`) contains:

    [database]
    path=/home/chris/Mail

    [new]
    tags=unread;inbox
    ignore=

    [search]
    exclude_tags=deleted;spam

I ran

    $ notmuch new

to import all new mail from my database, which was the 19K messages. I
think that took about 5 minutes.

# Post-sync hook

Rather than manually running `offlineimap` and `notmuch new` all the
time, instead you can put

    autorefresh = 1

under your `[Account]` setting in `.offlineimaprc`. That will make
Offlineimap run in one continuous process. I run it in screen for now,
but I will probably add it as a system script when I'm feeling
masochistic.

Another thing you can add to the `[Account]` section is a
`postsynchook`:

    postsynchook = /usr/bin/offlineimap-postsync

That path points to my post-sync script. It contains:

    notmuch new

And then a bunch of tagging commands.

# Tagging

In GMail I would organize everything with filters and tags. I like
this approach, so I took the same with Notmuch. First, mailing lists
skip the inbox and are tagged. For example:

    notmuch tag -inbox +ghc-devs to:ghc-devs@haskell.org tag:inbox
    notmuch tag -inbox +ghc-users to:glasgow-haskell-users@haskell.org tag:inbox
    notmuch tag -inbox +haskell-cafe to:haskell-cafe@haskell.org tag:inbox
    notmuch tag -inbox +haskell-beginners to:beginners@haskell.org tag:inbox

In other words, "remove the `inbox` tag, and add the `ghc-devs` tag
for all messages addressed to `ghc-devs@haskell.org` in my inbox."

When I receive an electric bill I tag it and flag it:

    notmuch tag +flagged +bill from:serviziweb@trenta.it tag:inbox

I also have some inbox skipping filters from lists or people I don't
have interest in seeing in my inbox.

Then I have 69 deletion filters on various mailing lists I never
signed up for and am unable to unsubscribe from.

In all I have about 130 filters. I copied them from my GMail account
and ran some keyboard macros to conver them to Notmuch's tagging
style.

# Emacs

Once you have Notmuch setup, you can use notmuch.el and it works out
of the box for reading and searching mail. The mode has some strange
choices for its defaults, so I copied
[the repo](https://github.com/chrisdone/notmuch) with full intention
for patching it heavily in the future, and I made some further
configurations
[in a separate file](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/notmuch.el).

The mode is pretty self-explanatory, it just has very silly
keybindings. Otherwise it works very well.

# Sending email

One thing that doesn't work out of the box is sending mail. For this I
configured my mail user agent:

    (setq mail-user-agent 'message-user-agent)

Set my default sending details:

    (setq user-mail-address "chrisdone@gmail.com"
          user-full-name "Chris Done")

Configured the SMTP server info for GMail:

    (setq smtpmail-stream-type 'ssl
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 465)

Then I made a `~/.authinfo` file and put in:

    machine smtp.gmail.com login chrisdone port 465 password "<password here>"

# Suave desktop panel

I also
[added the current inbox status to my Suave XMonad desktop panel](https://github.com/chrisdone/chrisdone-xmonad/blob/b097b98ac044fc1cfa0f2bccd9abe9803b6ac8c4/src/XMonad/Suave/Window.hs#L69). There's
a screenshot [here](http://chrisdone.com/suave-1.png). The inbox
display is in the centre.

# Summary

In summary I moved the client part of my GMail use from the GMail web
client to Emacs. Now I can read and write mail completely in Emacs,
and I can see when new mail has arrived in my panel.
