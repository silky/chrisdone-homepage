---
date: 2011-06-26
title: Some insights from The Mythical Man Month starting from Chapter 11
description: Some insights from The Mythical Man Month starting from Chapter 11
author: Chris Done
tags: programming, books
---

I recently read [The Mythical Man
Month](http://en.wikipedia.org/wiki/The_Mythical_Man-Month), a classic
book about software development.

I thought this quote was tres amusant:

> "The Bible of Software Engineering", because, "everybody quotes it,
  some people read it, and a few people go by it."

In my reading of the book, around chapter 11, “Plan to Throw One Away”,
I got the idea to annotate and underline sentences and paragraphs that
rang true with my experience or that I thought were insights that I
and everyone should take into account.

Now that I've finished it, I thought I'd jot those points, that I felt
the need to underscore, here. Flicking back through the earlier
chapters there are lots of other points I ought to underscore, but
that's for another time.

I often see or participate in debates about software development that
are better summed up by many clear insights from *MMM*, so it's good
for me to jot them down; having a common vocabulary and literature
avoids a bunch of redundant discussion. For example, I saw some rather
odd posts to Reddit's programming section with laboured gardening and
writing analogies.

I'm not sure what the legality of typing up so much of a book
is. There is a lot more context to each of the points below, so you
really need [the book](http://www.amazon.com/gp/reader/0201835959)
anyway to fully grok everything covered. Many points in the book may
or may not have been underscored depending on the availability of a
pen at the time, and I miss out the first ten chapters. Others
downright do not make sense without the context which I don't feel
comfortable in further quoting verbatim.

At any rate, most of the quotes below have been quoted verbatim
elsewhere.

## Plan to Throw One Away

### Pilot Plants and Scaling Up

* “Programming system builders have also been exposed to this lesson,
  but it seems to have not yet been learned. Project after project
  designs a set of algorithms and then plunges into construction of
  customer-deliverable software on a schedule that demands delivery of
  the first thing built.”
* “In most projects, the first system built is barely usable. It may
  be too slow, too big, awkward to use, or all three.”
* “The discard and redesign may be done in one lump, or it may be done
  piece-by-piece. But all large-system experience shows that it will
  be done.”
* “The management question, therefore, is not *whether* to build a
  pilot system and throw it away. You *will* do that. The only
  question is whether to plan in advance to build a throwaway, or to
  promise to deliver the throwaway to customers.”
* “Hence *plan to throw one away; you will, anyhow.*”

### The Only Constancy is Change Itself

* “But the very existence of a tangible object serves to contain and
  quantize user demand for changes.”
* “Clearly a threshold has to be established, and it must get higher
  and higher as development proceeds, or no product ever appears.”
* “The throw-one-away concept is itself just an acceptance of the fact
  that as one learns, he changes the design.”

### Plan the System for Change

* “Most important is the use of a high-level language and
  self-documenting techniques so as to reduce errors induced by
  changes. Using compile-time operations to incorporate standard
  declarations helps powerfully in making changes.”
* “Every product should have numbered versions, and each version must
  have its own schedule and a freeze date, after which changes go into
  the next version.”

### Plan the Organization for Change

* “[…] the reluctance to document designs is not due merely to
  laziness or time pressure. Instead it comes from the designer's
  reluctance to commit himself to the defense of decisions which he
  knows to be tentative.”
* “[M]anagers themselves often think of senior people as ‘too
  valuable’ to use for actual programming.”

### Two Steps Forward and One Step Back

* “The fundamental problem with program maintenance is that fixing a
  defect has a substantial (20-50 percent) chance of introducing
  another. So the whole process is two steps forward and one step
  back.”
* “If fact it often has system-wide ramifications, usually
  nonobvious. […] the far-reaching effects of the repair will be
  overlooked.”
* “Clearly, methods of designing programs so as to eliminate or at
  least illuminate side effects can have an immense payoff in
  maintenance costs. So can methods of implementing designs with fewer
  people, fewer interfaces, and hence fewer bugs.”

### One Step Forward and One Step Back

* “Soon or later the fixing ceases to gain any ground. Each forward
  step is matched by a backward one. Although in principle usable
  forever, the system has worn out as a base for progress.”
* “A brand-new, from-the-ground-up redesign is necessary.”
* “Systems program building is an entropy-decreasing process, hence
  inherently metastable. Program maintenance is an entropy-increasing
  process, and even its most skillful execution only delays the
  subsidence of the system into unfixable obsolescence.”

## Sharp Tools

* “A good workman is known by his tools.” (proverb)

### High-level Language and Interactive Programming

* “I cannot easily conceive of a programming system I would build in
  assembly language.”

## The other face

### Self-Documenting Programs

* (An almost meta-quote here considering the context of this post)
  “Refer to standard literature to document basic algorithms wherever
  possible. This saves space, usually points to a much fuller
  treatment than one would provide, and allows the knowledgeable
  reader to skip it with confidence that he understands you.”


## No Silver Bullet

### Does It Have to Be Hard?—Essential difficulties

* “First, we must observe that the anomaly is not that software
  progress is so slow, but that computer hardware progress is so
  fast.”
* “No other technology since civilization began has seen six orders of
  magnitude price-performance gain in 30 years.”
* “Second, to see what rate of progress we can expect in software
  technology, let us examine its difficulties. Following Aristotle, I
  divide them into *essense*—the difficulties inherent in the nature
  of the software—and *accidents*—those difficulties that today attend
  its production but that are not inherent.”
* “I believe the hard part of building software to be the
  specification, design and testing of this conceptual construct, not
  the labour of representing it and testing the fidelity of the
  representation. […] If this is true, building software will always
  be hard. There is inherently no silver bullet.”

### Complexity

* “Software entities are more complex for their size
  than perhaps any other human construct, because no two parts are
  alike (at least above the statement level). If they are, we make the
  two similar parts into one, a subroutine, open or closed. In this
  respect software systems differ profoundly from computers,
  buildings, automobiles, where repeated elements abound.”
* “Digital computers are themselves more complex than most things
  people build; they have very large numbers of states. This makes
  conceiving, describing, and testing them hard. Software systems have
  orders of magnitude more states than computers do.”

Below; functional programming springs to mind:

* “From the complexity comes the difficulty of enumerating, much less
  understanding, all the possible states of the program, and from that
  comes the unreliability.”
* “From complexity of structure comes the difficulty of extending
  programs to new functions without creating side effects. From
  complexity of structure comes the unvisualized states that constitute
  security trapdoors.”
* “The physicist labors on, however, in a firm faith that there are
  unifying principles to be found […] because God is not capricious or
  arbitrary. No such faith comforts the software engineer.”
* “[…] not because of necessity but only because they were designed by
  different people, rather than God.”
* “Partly this is because the software in a system embodies its
  function, and the function is the part that most feels the pressures
  of change. Partly it is because software can be changed more
  easily—it is pure thought-stuff, infinitely malleable.”
* “All successful software gets changed. Two processes are at work. As
  a software product is found to be useful, people try it in new cases
  at the edge of, or beyond, the original domain. The pressures for
  extended function come chiefly from users who like the basic
  function and invent new uses for it.”
* “As soon as we attempt to diagram software, we find it to constitute
  not one, but several, general directed graphs, superimposed on one
  another.”

I think the below is a very interesting point; having a visual mind
does not seem to help you in programming.

* “In spite of progress in restricting and simplifying the structures
  of software, they remain inherently unvisualizable, thus depriving
  the mind of some of its most powerful conceptual tools.”

Which follows nicely into the next point I underscored a page later:

### Graphical programming

* “Nothing even convincing, much less exciting, has yet emerged from
  such efforts. I am persuaded that nothing will.”

### Program verification

* “Program verification does not mean error-proof programs. There is
  no magic here, either. Mathematical proofs also can be faulty. So
  whereas verification might reduce the program-testing load, it
  cannot eliminate it.”
* “More seriously, even perfect program verification can only
  establish that a program meets its specification. The hardest part
  of the software task is arriving at a complete and consistent
  specification, and much of the essence of building a program is in
  fact the debugging of the specification.”

### Environments and tools

One point that reminded me of
[a recent post by Joe Armstrong to the Erlang mailing list](http://erlang.org/pipermail/erlang-questions/2011-May/058769.html):

* “Perhaps the biggest gain yet to be realized in the programming
  environment is the use of integrated database systems to keep track
  of the myriads of details that must be recalled accurately by the
  individual programmer and kept current in a group of collaborators
  on a single system.”

(And I don't think ‘intellisense’ really covers it.)

### Promising Attacks on the Conceptual Essense

I found this to be a very interesting perspective considering the era
in which it was written:

* “There are dramatic exceptions to my argument that the generalization
of the software packages has changed little over the years: electronic
spreadsheets and simple database systems. These powerful tools, **so
obvious in retrospect and yet so late appearing**
[bold added for emphasis], lend themselves to myriad uses, some quite
unorthodox.”

### Incremental development — grow, not build, software

* “Some years ago, Harlan Mills proposed that any software system
  should be grown by incremental development. That is, the system
  should first be made to run, even though it does nothing useful
  except call the proper set of dummy subprograms. Then, bit by bit it
  is fleshed out, with the subprograms in turn being developed into
  actions or calls to empty stubs in the level below.”
* “The morale effects are startling. Enthusiasm jumps when there is a
  running system, even a simple one.”
* “I find that teams can *grow* much more complex entities in four
  months than they can *build*.” (Yes, I see the gardener analogy
  here, but *please*.)

### Great designers

* “I think the most important single effort we can mount is to develop
  ways to grow great designers.”

There is a lot more crammed in this book, some several more
chapters. But I'll stop here.
