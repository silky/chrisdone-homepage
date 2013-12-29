---
date: 2013-12-25
title: Emacs users are like Terry Pratchett’s Igors
description: Emacs users are like Terry Pratchett's Igors
author: Chris Done
tags: emacs
---

Within the constraints of the Emacs environment, there are no
limits. Emacs is built upon this principle; the Lisp principle. Make
some small kernel of features. A substrate. Take care to make this
substrate programmable (in any language, Emacs chooses Lisp), and then
build the system upon it. Let users worry about future features. Some
359,000 lines of C code comprise its kernel, and 1,637,000 lines of
Emacs Lisp take the rest of it.[^1]

Similarly, the nature of The
[Lisp Curse](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html)
is that what can be written to express any given problem is so
arbitrary and free, that you are spoiled for choice, and every
programmer re-invents solutions to the same problems, uncaring about
sharing a common language with the world outside. The core problem is
that Lisp makes programmers selfish. Giving programmers so much
flexibility is inviting the alienation of other programmers; Lispers
think: how can I express this problem best, for me?

That phenomenon is not a problem for Emacs. Work environments are a
very personal thing. They exist to serve only one person:
you. Me-me-me is a winning attitude when dealing with your
environment. Maybe in the real world, out there, where you have to
share programs with other people, where other people will see or even
use your efforts, you have to care or take consideration about other
people. Not in your environment. Not in your Emacs. It is your virtual
home for at least nine hours of the day, every day.

My road to Emacs love developed slowly. I first came to it due to
Common Lisp. I knew enough Lisp to get by, copy-pasting example
snippets, configuring just enough to edit my environment. It felt a
little bit barebones compared to the IDEs I'd used before it. Little
did I know the world of functionality and configuration waiting
beneath my feet.

Eventually I started patching some things here and there, writing my
own hooks, little things like that. I used Emacs for a long time, just
becoming proficient as a user with the keybindings and window
management, before I ever wrote any Elisp. It hadn't occured to me
that writing any Elisp would ever be of interest to me. I would often
shaft my .emacs configuration, and everything would break, and I
wouldn't quite know why.

Finally, I wrote my first mode. I think it was a mode for ASP. It
wasn't very good, and I didn't fully understand everything that was
going on. But it gave me some key insights. This thing isn't just an
editor, it's really an environment all the way down. I can configure
_everything_ about this mode. And the mode consists of a bunch of
functions and variables. It's all code.

After that, it was really a sky-rocket of productivity. Eventually I
would write Elisp casually in between programming on work projects. I
would notice that a way of working was repetitive, or that Emacs
behaved in a way I just didn't quite like, or I simply thought of a
nice thing that I could add. I'd happily spend anywhere from 30
seconds to half an hour writing some functionality to extend my
editing facilities.

And it was extended for good. That amazed me, and still does. My
problems are only problems for as long as I don't notice them. Once I
do, I write some Elisp to solve it, and then it's never a problem
again. In typical editors and IDEs, I simply wouldn't even think of
fixing such things, never mind actually putting my work to one side
for a minute, solving them, and then going back to work again.

I've now written a lot of Elisp to support my development, especially
with respect to Haskell. Many times, for many months at a time, over
the years, I've been working on an experimental feature, or feature
set, mode, what-have-you, and it's been very spotty. Mostly working,
but breaking a lot, interrupting my work, but with me persevering,
pushing through, until that work becomes stable and quite robust
through sheer usage and battle testing.

When working recently it occured to me that a lot of the functionality
I depend on presently in Emacs for work is built upon my own work. I
use the session/interactive-mode work for interacting with Cabal and
GHCi, I use structured-haskell-mode in conjunction with that, and then
atop that I use god-mode, my own Emacs input method. At one time or
another in the past they have all been unusable, or flaky as hell. SHM
still has a few growing pains, but is basically there.

This really reminds me of Terry Pratchett's
[Igor](http://en.wikipedia.org/wiki/Igor_%28Discworld%29) clan. I
discovered this amiable race in The Fifth Elephant. Igors are a people
inspired from the typical hunchbacked Igor archetype, but in
Discworld, they are also self-modifiers. Their bodies consist of mixed
and matched and patched and swapped body parts among other members of
their clan, of scars and self-adjustements. They are infinitely
self-improving, self-experimenting. They might end up with a botched
job and have to hobble around for a few days, but in the end it's
always fixable.

And they lisp.

[^1]: Though, while Elisp is Emacs's programmability language
      of choice, the particular language doesn't matter much. It could
      be Python, JavaScript, Haskell, whatever. The key is: if most of
      your feature set is written in your editor's programming
      language, then that editor is very programmable.
