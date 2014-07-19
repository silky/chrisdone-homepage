---
date: 2014-07-19
title: Teaching: A good (and bad) example
description: Teaching: A good (and bad) example
author: Chris Done
tags: education
---

Rather than write a bunch of pros about how I think teaching should be
done, this time I'll simply comment an existing example. Needless to
say, I am a proponent of
[the Socratic method](http://en.wikipedia.org/wiki/Socratic_method). As
I begin more work in the area of education, I'll refer back to this
page.

One day, a newbie asked on an IRC channel a Haskell question. Here is
a highly editorialized and commentaried version of the conversation,
correcting typos and patching out the names. I tried to also group
conversations so they're a bit less interleaved.

It doesn't really matter who is who, but I wanted to include a real
log to show that there are real examples of this all the time. It
demonstrates some points about teaching that I think are
important. I've used names for some philosophers I admire. There isn't
(really) any meaning to the assignments.

# The Dialogue

The play starts out with an unknown person joining the channel and
asking for help:

> * **Hos**: Hey guys, quick question for you. I am working on
>   expanding my Haskell skills by doing some challenges at
>   https://www.hackerrank.com, and I am having issues with one thing
>   that I think has got to be much easier than I am making it.
> * **Huizi**: Reading and parsing `STDIN`.

The canvas is prepared. It's not clear what exactly they are having
trouble with. What's the first thing you should do? Ask them
questions.

> * **Socrates**: Do you want to avoid direct spoilers?
> * **Aristophanes**: Huizi: Sure, can you give a specific example
>   of something you'd like to do?

Questions begin. This is a good start.

> * **Huizi**: So, say I am expecting a string of `"2 3"`, how would I
>   convert that to a `[Int]`?

A very specific, easy to investigate question is proposed. What's the
best approach to proceed? First, establish the person's experience.

> * **Aristophanes**: `map read . lines`
> * **Aristophanes**: Err.
> * **Laozi**: `map read . words <$> readLine`
> * **Confucius**: You mean `words`.
> * **Confucius**: And `getLine`, not `readLine`.
> * **Socrates**: Huizi, scratch that question about direct
>   spoilers. The answer's been given verbatim.

Sadly, the response is not helpful. It is unempathic, it
presumptuous and muddled. Naturally, the learner is confused by
this noise.

> * **Huizi**: What is the `.` in there for?

And their struggle continues:

> * **Confucius**: Huizi: Function composition.
> * **Laozi**: `:t (.)`
> * **Minerva**: `(b -> c) -> (a -> b) -> a -> c`

So far, the poor learner has been subjected to line noise. But it
doesn't go unnoticed:

> * **Socrates**: Hmm, newbie handling in here isn't as awesome as
>   back in 2008. Rather clumsy. [winking]
> * **Socrates**: Huizi, how much Haskell do you know already?

Amusingly, nor does the criticism, starting a separate
meta-discussion:

> * **Laozi**: Socrates: What exactly is not satisfactory to you?
> * **Confucius**: Socrates: I doubt this is actually the puzzle
>   Huizi is trying to solve. [tongue in cheek]

The important reply of the earlier question comes:

> * **Huizi**: I have ran through a lot of sample projects, involving list manipulation.
> * **Huizi**: So, I would still consider myself a noob.

In other words, this is a complete Haskell newbie who needs to read
more material, or, if they are to be taught here, it should be done with
care and patience.

> * **Confucius**: Huizi: Another way is to write something like
>   `main = do xs <- getLine; let ns = map read (words xs); …` use `ns`
>   here …

Oddly, the spoilering continues. The learner has already stated that
they're going through exercises. The channel is persisting in trying
to spoil the learning process and it does so clumsily. The
meta-discussion continues:

> * **Socrates**: Laozi, the part where you give a complete newbie
>   some spoiler code that mixes function composition with `Applicative`
>   operators? [wink]
> * **Confucius**: Yeah, if I was going to use `fmap`, I'd just write
>   `fmap` here.
> * **Confucius**: I only ever use `<$>` if I'm also using `<*>`.

The learner expresses bad insights typical of someone unfamiliar with
a topic:

> * **Huizi**: But I can work with the language. Just anything with
>   reading input or output just feels unnatural in the language.
> * **Huizi**: But that's likely because it is unnatural for
>   functional programming languages.
> * **Confucius**: Huizi: Eh, it's not so bad, you just need to
>   get used to it.

A strange selfish approach to teaching is expressed in reply to
criticism in the meta-discussion:

> * **Laozi**: Socrates: They are free to ask for more help, I
>   don't like starting off assuming no knowledge, it's too much work
>   for me.
> * **Socrates**: Laozi, you make it sound like an occupation. [smile]
> * **Laozi**: Socrates: 0K starting. [money job joke]

Amusingly, the shoot-first-ask-questions-later practitioner
second-guesses themself:

> * **Laozi**: Also, is it spoiler code? Is the actual task to parse in
>   some numbers?
> * **Confucius**: Huizi: Have a look at the code that I wrote.

Incensed by the learner's bad learner insights, a
pure-functional-vs-state lecture begins:

> * **Aristophanes**: Huizi: It's only "unnatural" in the sense
>   that it's a different approach than imperative languages, and
>   that's the approach you already know.
> * **Huizi**: No, its unnatural in the sense that functional
>   programming languages have this sense of no outside
>   state... reading from `STDIN` kind of "breaks" that whole ideology.

Meanwhile, an interaction with the learner is resumed:

> * **Socrates**: Huizi, are you just going off the reading material
>   on that challenge site so far?
> * **Nasreddin**: Huizi: FWIW, parsing numbers from strings is explained on page 400 in Real World Haskell: http://book.realworldhaskell.org/read/
> * **Huizi**: Socrates: No, I was using ‘learnhaskell’ sites.

The lecture continues:

> * **Confucius**: Huizi: A do-block can be used to glue a bunch of IO
>   actions together into a larger one. Inside a do-block, `v <- x`
>   means "execute the action `x`, and call its result v". If `x` has type
>   `IO t`, then `v` will have type `t`.
> * **Aristophanes**: Huizi: The whole "no sense of outside state"
>   thing is basically a myth.
> * **Confucius**: Huizi: In the end, main will be one of these IO
>   actions, and will be executed in a compiled Haskell program.

The learner soldiers on trying to solve their actual problem:

> * **Huizi**: Yeah, I was trying to use `read`, I just kept getting
>   errors.

More lecturing:

> * **Confucius**: `getLine :: IO String` is an IO action which when
>   executed, will request a line of text from `stdin`, and produce
>   that as its result.

Actual interaction with the learner tries to make some headway:

> * **Socrates**: Huizi, what was your use of `read` like? You can
>   type small Haskell expressions in here by prefixing it with "> ".
> * **Socrates**: > `1 + 2`
> * **Minerva**:  `3`

Lecture *still* continues:

> * **Confucius**: Huizi: It's important to distinguish execution (of
>   IO actions) from evaluation, which is the process of reducing
>   expressions to values in order to be able to match them against
>   patterns.
> * **Confucius**: When people say that Haskell is pure or
>   referentially transparent or stateless or whatever, they're
>   talking about evaluation.

The learner is now trying to diagnose their problem and voicing their
discoveries:

> * **Huizi**: Looks like the main issue I was having is that I was
>   using `splitOn ' '`, instead of words.

This is the first obvious clue to a misstep the newbie has made. Can
you see their mistake? If not, you'll learn something, too. Now the
task is to help them realize it. Meanwhile, a little bit more meta
discussion:

> * **Socrates**: Confucius, does your approach of throwing stuff at
>   people and seeing what sticks without any feedback normally work?
>   It seems strange to me.
> * **Confucius**: Socrates: I am hoping for feedback here.

The learner is starting to reason through the issue more:

> * **Huizi**: `splitOn` must have a different output type.
> * **Socrates**: Huizi, what's the type of `splitOn`? Is that from `Data.List.Split`?
> * **Huizi**: Yeah.
> * **Socrates**: `:t splitOn` *-- Does lambdabot have it in scope?*
> * **Minerva**: `Eq a => [a] -> [a] -> [[a]]`
> * **Socrates**: Aha!

A strange lecture-only perspective is professed, which I'll address
later:

> * **Confucius**: Socrates: But I figure I might as well put
>   everything on the table even if I get none.
> * **Confucius**: Huizi: Does that stuff make sense?

Attempting to continue with the issue at hand, a play is made to try
to help the learner see what's wrong:

> * **Socrates**: Huizi, you can still use `splitOn` for this problem
>   (even though `words` might be more convenient), can you see the
>   change to make in your call?
> * **Huizi**: `:t words`
> * **Minerva**: `String -> [String]`
> * **Socrates**: Huizi, check this out:
> * **Socrates**: `:t ' '`
> * **Minerva**: `Char`
> * **Socrates**: `:t " "`
> * **Minerva**: `[Char]`

But the learner's still confused, but asking questions, which is good:

> * **Huizi**: So basically I end up with an output of `[[Char]]` with
>   words, and `[[a]]` with `splitOn`. Right?
> * **Huizi**: But since the input to `splitOn` is from the `getLine`,
>   does it not consider itself a `[Char]` type?
> * **Socrates**: Huizi, that's right, `splitOn` returns a list of
>   lists of anything `[a]`, `words` returns a list of lists of
>   characters `[Char]`.
> * **Nasreddin**: Huizi: `splitOn` would give `[[Char]]` if it was
>   fed `[Char]`

Leading the learner isn't working yet, so it's time to show the issue:

> * **Socrates**: Huizi, it does -- I think the *first* argument to `splitOn` is your problem here, right? Is this the error message you got?
> * **Socrates**: `> splitOn ' ' "hello world"`
> * **Minerva**:  Couldn't match expected type ‘[Char]’ with actual type ‘Char’
> * **Confucius**: `> splitOn " " "hello world"`
> * **Minerva**:  `["hello","world"]`

Here comes the epiphany!

> * **Huizi**: Ohhhhhh.
> * **Socrates**: [smile]
> * **Huizi**: I feel silly.

Then they try it out:

> * **Huizi**: `> splitOn "o w" "hello world"`
> * **Minerva**:  `["hell","orld"]`

Now they describe the problem they had with full understanding:

> * **Huizi**: Ok, so I am splitting the list wherever it finds the sequence in that list.
> * **Socrates**: Yup.
> * **Huizi**: `> splitOn [2,3] [1,2,3,4,5,6]`
> * **Minerva**:  `[[1],[4,5,6]]`
> * **Huizi**: Perfect, thanks guys.
> * **Socrates**: Welcome!

End of conversation.
