---
date: 2011-10-10
title: Names in software systems
author: Chris Done
tags: software, culture
---

Another idea for another time.

It's [hard to get right
internationally](http://www.w3.org/International/questions/qa-personal-names). I
would perhaps try a form like this:

    Full name: [ ]

    Examples:

        毛泽东

        Björk Guðmundsdóttir

        Christopher Smith

        Isa bin Osman

        María-Jose Carreño Quiñones

    For correctly sorting your name amongst others, it helps us if you
    can enter the parts of your name significant for searching. For
    example, how would your name be sorted on a list?

    Parts: [ ] [ ] [ ] [+] (in order of importance in your culture)

    Examples:

        [毛] [泽] [东]

        [Björk] [Guðmundsdóttir]

        [Smith] [Christopher]

        [Isa] [Osman]

        [Carreño Quiñones] [María-Jose]

    How do we refer to you in correspondence?

    Correspondance name: [ ]

    Examples:

        毛先生

        Björk

        Mr Smith or Christopher

        Encik Isa

        Señorita Carreño

And then make this a standard (jQuery or w/e) library so I don't have
to write it ever again. You could do some limited detection and
autofill with suggestions, e.g. if you detect Chinese then you can
assume the order is the same, for an Englishy sounding name, you could
venture reversing it. If you find [bin], [van], etc. you might
consider dropping those. The more people contribute to the library,
the better it can be at guessing name structure and the less work
users have to do, but it still lets users tweak it to be correct.
