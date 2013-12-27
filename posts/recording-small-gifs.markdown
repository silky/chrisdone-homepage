---
date: 2013-12-09
title: Recording simple GIFs for demos
description: Recording simple GIFs for demos
author: Chris Done
tags: emacs
---

Sometimes you might like to record little GIF animations of your
screen to demonstrate an Emacs feature you did (hey, some of you
might…). For example,
[these](https://github.com/chrisdone/structured-haskell-mode#features). I made
a wee F9 keybinding for Emacs to run this:

``` lisp
(defun screenshot-frame ()
  "Take a screenshot of 400x200 pixels of the Emacs frame."
  (interactive)
  (shell-command-to-string
   (concat "sleep 1; "
           "import -window 0x3c000a3 "
           "-crop 400x200+13+0 +repage /tmp/frames/`date +%s`.png")))
```

Replace the window id with the window id of your target window, which
you can get with `xwininfo -display :0`.

I would execute `screenshot-frame` after running a command or pressing
a key (it sounds painful but it's not, and it allows you to make
mistakes). The `sleep` call is to ensure that the buffer has finished
updating. I also disabled `blink-cursor-mode`. Then to preview the
animation so far I would use

    animate -delay 35 /tmp/frames/*.png

If some frames were redundant I'd remove them. And then finally to write out a .gif I'd use

    convert -delay 35 /tmp/frames/*.png out.gif

I found the whole thing quite convenient!
