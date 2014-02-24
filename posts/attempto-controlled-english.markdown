---
date: 2014-02-24
title: Attempto Controlled English
description: Attempto Controlled English
author: Chris Done
tags: haskell, ace, linguistics
---

[Attempto Controlled English](http://en.wikipedia.org/wiki/Attempto_Controlled_English)
is a formally defined unambiguous language which is a subset of the
English language. It's pretty sweet.

I've known about it for some time, but I never fiddled with it because
the standard implementation setup is rather elaborate. I wanted a
nice, simple package in Haskell which would define a parser and a
printer only, much like haskell-src-exts does. That way I can use ACE
to parse some simple English for all sorts of purposes[^1], with a simple
familiar API that I can peruse on Hackage. Partly it's also a good
learning experience.

So I went through the paper The Syntax of Attempto Controlled English
to see whether it was comprehensive enough to write a parsec parser
out of. It was! I first wrote a
[tokenizer](https://github.com/chrisdone/ace/blob/master/src/ACE/Tokenizer.hs)
in with Attoparsec and
[wrote some tests](https://github.com/chrisdone/ace/blob/master/test/Main.hs#L39). From
those tokens I produced a set of
[combinators](https://github.com/chrisdone/ace/blob/master/src/ACE/Combinators.hs)
for Parsec, then I wrote a
[parser](https://github.com/chrisdone/ace/blob/master/src/ACE/Parsers.hs). While
writing the parser I produced a set of
[test-cases](https://github.com/chrisdone/ace/blob/master/test/Main.hs#L67)
for each grammar production.  Finally, I wrote a
[pretty printer](https://github.com/chrisdone/ace/blob/master/src/ACE/Pretty.hs),
and
[wrote some tests](https://github.com/chrisdone/ace/blob/master/test/Main.hs#L599)
to check that `print . parse . print . parse = id`.

Newbies to Haskell parsing might find it an interesting use-case
because it tokenizes with
[Attoparsec](http://hackage.haskell.org/package/attoparsec) (from
Text) and then parses its own token type
([Token](http://hackage.haskell.org/package/ace-0.3/docs/ACE-Types-Tokens.html))
with [Parsec](http://hackage.haskell.org/package/parsec). A common
difficulty is to avoid parsing from `String` in Parsec, which most
tutorials use as their demonstration.

The Hackage package is
[here](http://hackage.haskell.org/package/ace). I find the
documentation interesting to browse. I tried to include helpful
examples for the production rules. You shouldn't have to know syntax
theory to use this library.

Here is an ACE sample. We can parse the sentence "a &lt;noun&gt;
&lt;intrans-verb&gt;" like this:

``` haskell
λ> parsed specification "a <noun> <intrans-verb>."
Right (Specification (SentenceCoord (SentenceCoord_1 (SentenceCoord_2
(SentenceCoord_3 (TopicalizedSentenceComposite (CompositeSentence
(Sentence (NPCoordUnmarked (UnmarkedNPCoord (NP (SpecifyDeterminer A)
(N' Nothing (N "<noun>") Nothing Nothing Nothing)) Nothing))
(VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
[])))))) Nothing) Nothing) Nothing) Nothing) Nothing)
```

Anything to do with vocabulary is written as `<foo>`. The parser
actually takes
[a record of parsers](http://hackage.haskell.org/package/ace-0.3/docs/ACE-Parsers.html#t:ACEParser)
so that you can provide your own parsers for each type of word. These
words are not of interest to the grammar, and your particular domain
might support different types of words.

If we pretty print the parsed phrase, we get:

``` haskell
λ> fmap pretty (parsed specification "a <noun> <intrans-verb>.")
Right "a <noun> <intrans-verb>."
```

I.e. we get back what we put in. I also wrote a HTML printer. A more
complicated sentence demonstrates the output:

> for each &lt;noun&gt; &lt;var&gt; if a &lt;noun&gt; that
> &lt;trans-verb&gt; some &lt;noun&gt; and &lt;proper-name&gt;'s
> &lt;noun&gt; &lt;trans-verb&gt; 2 &lt;noun&gt; then some
> &lt;noun&gt; &lt;intrans-verb&gt; and some &lt;noun&gt;
> &lt;distrans-verb&gt; a &lt;intrans-adj&gt; &lt;noun&gt;
> &lt;proper-name&gt;'s &lt;noun&gt; &lt;adverb&gt;.

Can be printed with

``` haskell
fmap (renderHtml . toMarkup) . parsed specification
```

and the output is:

<blockquote><p><span title="specification" class="ace-specification"><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="universal-topic" class="ace-universal-topic"><span title="universal-quantor" class="ace-universal-quantor"><span title="universal-quantor" class="ace-universal-quantor">for each</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span> <span title="appos-coord" class="ace-appos-coord"><span title="apposition" class="ace-apposition"><span title="variable" class="ace-variable">&lt;var&gt;</span></span></span></span></span> <span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="conditional" class="ace-conditional"><span title="if-if" class="ace-if-if">if </span><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence" class="ace-sentence"><span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="determiner" class="ace-determiner">a</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span> <span title="relative-clause-coord" class="ace-relative-clause-coord"><span title="relative-clause" class="ace-relative-clause"><span title="relative-clause-that" class="ace-relative-clause-that">that </span><span title="vp-coord" class="ace-vp-coord"><span title="vp" class="ace-vp"><span title="v_" class="ace-v_"><span title="compl-v" class="ace-compl-v"><span title="transitive-v" class="ace-transitive-v">&lt;trans-verb&gt;</span> <span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="determiner" class="ace-determiner">some</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span><span title="unmarked-npcoord-and" class="ace-unmarked-npcoord-and"> and </span><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="possessive-np-coord" class="ace-possessive-np-coord"><span title="proper-name" class="ace-proper-name">&lt;proper-name&gt;</span><span title="genitive-tail" class="ace-genitive-tail"><span title="genitive-tail" class="ace-genitive-tail"><span title="saxon-genitive-marker" class="ace-saxon-genitive-marker">&#39;s</span></span></span></span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span></span></span></span></span></span></span></span></span></span> <span title="vp-coord" class="ace-vp-coord"><span title="vp" class="ace-vp"><span title="v_" class="ace-v_"><span title="compl-v" class="ace-compl-v"><span title="transitive-v" class="ace-transitive-v">&lt;trans-verb&gt;</span> <span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="number-p" class="ace-number-p">2</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span></span></span></span></span></span></span></span></span></span><span title="if-then" class="ace-if-then"> then </span><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence-or" class="ace-sentence-or"><span title="sentence-and" class="ace-sentence-and"><span title="sentence" class="ace-sentence"><span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="determiner" class="ace-determiner">some</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span> <span title="vp-coord" class="ace-vp-coord"><span title="vp" class="ace-vp"><span title="v_" class="ace-v_"><span title="compl-v" class="ace-compl-v"><span title="intransitive-v" class="ace-intransitive-v">&lt;intrans-verb&gt;</span></span></span></span></span></span><span title="sentence-op" class="ace-sentence-op"> and </span><span title="sentence-and" class="ace-sentence-and"><span title="sentence" class="ace-sentence"><span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="determiner" class="ace-determiner">some</span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span> <span title="vp-coord" class="ace-vp-coord"><span title="vp" class="ace-vp"><span title="v_" class="ace-v_"><span title="compl-v" class="ace-compl-v"><span title="distransitive-v" class="ace-distransitive-v">&lt;distrans-verb&gt;</span> <span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="determiner" class="ace-determiner">a</span></span> <span title="n_" class="ace-n_"><span title="adjective-coord" class="ace-adjective-coord"><span title="intransitive-adjective" class="ace-intransitive-adjective">&lt;intrans-adj&gt;</span></span> <span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span> <span title="npcoord" class="ace-npcoord"><span title="np" class="ace-np"><span title="specifier" class="ace-specifier"><span title="possessive-np-coord" class="ace-possessive-np-coord"><span title="proper-name" class="ace-proper-name">&lt;proper-name&gt;</span><span title="genitive-tail" class="ace-genitive-tail"><span title="genitive-tail" class="ace-genitive-tail"><span title="saxon-genitive-marker" class="ace-saxon-genitive-marker">&#39;s</span></span></span></span></span> <span title="n_" class="ace-n_"><span title="n" class="ace-n">&lt;noun&gt;</span></span></span></span></span> <span title="v-modifier" class="ace-v-modifier"><span title="adverb-coord" class="ace-adverb-coord"><span title="adverb" class="ace-adverb">&lt;adverb&gt;</span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span></span><span title="period" class="ace-period">.</span></span></p></blockquote>

The colors and parenthesizing embellishments are just to demonstrate
what can be done. I'm not sure this output would actually be readable
in reality.

This is a good start. I'm going to leave it for now and come back to
it later. The next steps are: (1) write more tests, (2) add feature
restrictions and related type information in the AST, (3) add a couple
sample vocabularies, (4) implement the interrogative (useful for query
programs) and imperative moods (useful for writing instructions,
e.g. text-based games).

[^1]: Specifically, I want to use this to experiment with translating
it to logic-language databases and queries, and from that produce
interactive tutorials, and perhaps experiment with a MUD-like game
that utilizes it.
