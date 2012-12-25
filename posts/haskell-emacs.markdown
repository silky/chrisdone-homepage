---
date: 2011-08-21
title: Haskell-emacs
author: Chris Done
tags: haskell, emacs
---

<!-- Part 1: Introduce the topic -->

## Introduction

I am currently working on, and using, an Emacs package called
[haskell-emacs](https://github.com/chrisdone/haskell-emacs). It is
intended as an eventual general replacement for
[haskell-mode](http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs)[^1]. It
won't be released officially for a while.

<!-- Part 2: Overview of the post -->

I will describe my motivations and development process, the current
features (with screenshots!) of this package, and the features
intended for the future.

<!-- Part 3: Motivations and development process -->

## Development Process

The reason for creating a new project and not patching the existing
haskell-mode is that I find its codebase too disjointed and fairly
messy to work with, and that it has different motivations and concerns
than mine.[^2]

My development process is threefold:

* Hack, hack, hack on Haskell.[^4]
* Notice that some operation is inconvenient for my workflow.
* Hack, hack, hack on Emacs.[^3]
* Start using the new Emacs feature.

It is for this reason that a lot of the features seem pointless or not
so useful to other people[^5], and the project is clearly focused on my
personal needs rather than general Haskellers but that doesn't concern
me too much. I'd expect there to be a substantial overlap, anyway.

<!-- Part 4: Overview of feature set -->

## Overview of Feature Set

As a general description of the package, the main features are:

* A Haskell editing mode.
* A Haskell interactive mode.
* Support for associating open Haskell buffers[^6] and interactive
  buffers with a common name, which are called “projects”.
* Some awareness of Cabal files and support for cabal-dev.

I will outline each of these features with example screenshots, by
going from start to finish of starting a project and doing some
hacking.

<!-- Part 5: Starting a project -->

## Starting a project

When you open a Haskell buffer or perform an action that needs a
project to continue, based on the current directory and whether
there's a .cabal file in it[^8], you will be prompted to start a new
project. In this case I've opened a file
`~/Projects/me/amelie/src/Main.hs` and there is a .cabal file in
`~/Projects/me/amelie`, so it infers the name of the project.

<img src="/images/hs-start-project.png"/>

Then it'll prompt for the Cabal file/dir to use. It defaults to
something sensible and displays why it's sensible (i.e. it has a
.cabal file).

<img src="/images/hs-cabal-dir.png"/>

Finally it prompts for what "load" directory to use for the
interactive mode (GHCi), with some sensible default[^9].

<img src="/images/hs-load-dir.png"/>

Once this is set, a GHCi REPL[^10] is opened. My project is now
"started", the file I originally opened is open and I have an
interactive REPL buffer. Additionally, a TAGS[^14] file is created
based on the contents of the project.

<img src="/images/hs-project-started.png"/>

(Ignore the double welcome message, that's a trivial bug I haven't
cared to fix.)

<!-- Part 6: Explain the REPL -->

## The REPL

A first test of the REPL shows a few things immediately:

* The prompt is merely a lambda.[^11]
* Warnings are collapsed into basically nothing.[^12]
* Warnings are displayed *before* the output of the expression.[^13]

<img src="/images/hs-repl-first-test.png"/>

As mentioned previously, a TAGS file was generated when the project
was started, so completion is available. The following name came
from the Main.hs file.

<img src="/images/hs-repl-completion.png"/>

When in the REPL, if a badly typed expression is evaluated, an error
buffer is raised containing the complete error message, and, if
location information is available, that part of your REPL prompt is
highlighted as an error.

<img src="/images/hs-repl-error-demo.png"/>

Hitting the `q` key closes this buffer and returns you to the REPL,
with no errors listed in your REPL. You can correct your expression,
and then everything is OK[^15].

<img src="/images/hs-repl-error-ok.png"/>

Hit `C-c C-k` to clear the REPL.

Regular GHCi commands can be used here:

<img src="/images/hs-repl-regular-ghci-stuff.png"/>

Compile errors are shrunk significantly in the REPL[^20]. For example, to
load a file into the REPL, hit `F5`[^16]. With a type error, this will
be displayed in the REPL and the minibuffer[^18][^17], note that the error is
substantially smaller than usual GHC output:

<img src="/images/hs-repl-load-error.png"/>

Putting the cursor on that error line and hitting return will take you
to the line and column in that file.

When you load a file successfully, there is no output in the REPL,
merely an "OK" message in the minibuffer.

When you try to do something that requires an extension, GHC usually
suggests which extension you need. Thanks to that, the loading feature
can also prompt for you to add that extension to your file. So if I
load a file that's trying to use Typeable deriving, I get this:

<img src="/images/hs-lang.png"/>

<!-- Part 7: Explain editing -->

## Editing

### Indentation

I haven't yet added support for the various indentation modes from
haskell-mode, because I don't understand them and I don't think
they're so great. I made a start a while ago on a set of test cases
for an indentor[^19], but for now I put up with basic Emacs editing
facilities. There is more work to do on this part of the project.

`C-<left/right>` re-indents the whole block below the current
line's starting column. For example:

<img src="/images/hs-edit-reindent-block.png"/>

Now consider I rename `fooBar` to `foo`. I end up with this:

<img src="/images/hs-edit-reindent-block-change-foo.png"/>

In my mode, I can go to the top line and hit `C-<left>` thrice and it
moves everything to:

<img src="/images/hs-edit-reindent-block-change-foo-reindent.png"/>

Without having to move the cursor at all. I've found this kind of
feature indispensable once I have it.

### Imports

For an import list, e.g.

<img src="/images/hs-edit-imports.png"/>

it is possible to hit `C-c C-.` which alphabetically sorts those imports
and spaces them out to be readable[^21]:

<img src="/images/hs-edit-imports-formatted.png"/>

<!-- Part 8: Explain completion -->

## Completion and Jumping

I use [Auto Complete](http://www.emacswiki.org/emacs/AutoComplete) for
emacs for completion, and an etags module so that I can provide it
with completions from a TAGS file. The TAGS file is updated when you
load the project for the first time and every time you save a
file[^22].

The sources for the completion are:

* Standard Haskell keywords
* LANGUAGE modes
* Standard functions
* Anything from the TAGS file
* Text from the current buffer(s)

Thanks to the TAGS file, we get jumping to definition for free. Hit
`M-.` to jump to the definition of the current symbol.

<img src="/images/hs-edit-jump-to-def.png"/>

And you will be taken to the definition of that function:

<img src="/images/hs-edit-jump-to-def-jumped.png"/>

Another form of completion is GHCi-based which completes when you
press space after any symbol.

<img src="/images/hs-edit-ghci-complete.png"/>

The type appears at the bottom. This is avoids mental context
switching when trying to remember the type of a function.

<!-- Part 8: Cabal utilities -->

## Cabal

There is support for .cabal files (taken from the haskell-mode
distribution), there is nothing interesting in this mode; it just does
highlighting.

In any buffer, including the interactive mode or cabal file, `C-c C-c`
will build the current project's Cabal package.

<img src="/images/hs-cabal-build.png"/>

Notice also that the build output is substantially reduced[^23].

`C-c c` similarly will prompt interactively for a Cabal command to
run, which uses ido-mode to complete-as-you-type. These cabal commands
will use `cabal-dev` if that is specified in the config previously
mentioned.

<img src="/images/hs-cabal-interactively.png"/>

So if you choose `configure`, it will run `cabal configure`:

<img src="/images/hs-cabal-configure.png"/>

There is really very little that you need to outside of Emacs. I have
even added a command for running non-Cabal commands, called “scripts”,
which I have bound to `C-c t`:

<img src="/images/hs-cabal-script.png"/>

Which runs any commands, in your `hs-config-scripts` list[^24], from
the project's Cabal directory. This is useful for,
e.g. restarting/refreshing services, running misc build scripts that
you want to run for this particular project.

Further to that, it's possible to define arbitrary commands to run in
the background, for which you don't care about the output, you just
want to know when it completes, e.g., rebuilding your JavaScript or
whatever. Here I am binding F5 to rebuild my JavaScript files in
espresso-mode, which is Emacs's JavaScript mode.

    (define-key espresso-mode-map (kbd "<f5>")
                '(lambda ()
                   (interactive)
                   (when (buffer-modified-p)
                     (save-buffer))
                   (hs-process-background-arbitrary-command
                    (hs-project)
                    ":!cd ../ && scripts/client-dev-build")))

<!-- Part 8: Elaborate on TODO list and future work -->

## Future Work

There are many things to do in the future that are feasible for me to
implement but just need doing. Some of these are:

* Debugger stepping support[^25]
* Indentation that doesn't suck[^19]
* Syntax-highlighted REPL prompt[^26]
* Show-based value inspection[^27]
* Interactive creation/management of Cabal file
* Automatic insertion of modules and Cabal dependency based on
  function/type use
* Haskell-aware code-folding
* Documentation of symbol at point
* Hoogle search support (I want to use something like [anything.el](http://www.emacswiki.org/emacs/Anything) for this)
* Hayoo search support (and this)
* Compilation on an interval
* Put hint suggestions in build/load output after warnings
* Ability to browse Haddock documentation inside Emacs (possibility
  for texinfo here)
* Support imenu so that we can use quickbar as a browser

Integration with Scion is an important aim, too. It would mean
depending on GHC7+, but we all have to make that transition at some
point.

At the top of my list right now is to use the index all of the
currently installed packages and hoogle/hayoo to be able to support
the automatic import and dependency insertion.

<!-- Footnotes -->

[^1]: Although for me it has replaced it already and I am using it at
      work and home.

[^2]: The editing mode puts a lot of effort into supporting both
      normal Haskell and literate Haskell which makes the code a confusing
      mess. I don't care about editing literate Haskell, and if I did
      I wouldn't create a mode like for normal Haskell files.

[^3]: Depending on what I want, this can take any time between 5
      minutes and a day.

[^4]: I use Haskell at work from around 8:00-18:00 every day, so any
      small annoyance becomes a big annoyance and a development
      problem that I need to address quite quickly.

[^5]: For example, in asking Haskellers who use Emacs at
      [CamHac](http://www.haskell.org/haskellwiki/CamHac), I found
      that people tended not to care too much about having multiple
      projects, and therefore REPLs, open simultaneously, because you
      can just “switch projects” by loading a different file into
      GHCi. I don't find this sufficient because I like my REPL
      history and output to be separate, and with
      [cabal-dev](http://hackage.haskell.org/package/cabal-dev) it's
      very important to have separate instances of GHCi.

[^6]: For non-Emacs users, a buffer is something that a “window” can
      display. A “window” in Emacs is some rectangle on the screen
      that you can resize or remove, like windows (or “tabs”) inside
      Eclipse or Visual Studio or whatnot. Common buffers are files,
      compilation output, etc. and you display them in various windows
      arranged on the screen. There is no conceptual difference to
      popular IDEs, but the naming convention can be confusingly
      different.

[^7]: Which is defective, actually, but not defective enough for me to
      care to fix it, over hacking on more useful features elsewhere
      in the project, at this point. “Patches welcome.”

[^8]: I'm making an assumption here that all Haskell projects have a
      .cabal file. I think it's a safe assumption to make. All useful
      Haskell projects eventually have a .cabal file.

[^9]: I don't remember at the time of writing whether this uses the
      src described in the .cabal file; I don't think so, as there
      could be many of these entries.

[^10]: Depending on whether you have set `hs-config-use-cabal-dev` to
       `t` or `nil`, it will start a cabal-dev-based GHCi process.

[^11]: This choice is because I want more space in my prompt, because
       I rarely care to see which module I'm in as it's obvious, and
       because lambda is pretty.

[^12]: This particular warning is an extreme example. I've come to a
       point personally where I just need to see the type and I know
       it's a defaulting problem. I will in the future add some
       description to this warning message. It was intended to have,
       but I think at some point I forgot to do it and it didn't
       bother me.

[^13]: This is an incidental side effect, which I've taken to. I
       explain later why this happens.

[^14]: A TAGS file is an file for Emacs based on ctags for Vim,
       which is a list of symbols and meta data about them that your
       editor can use for semantic searching, jumping and completing.

[^15]: And all your buddies viewing your REPL history will think you
       never make any mistakes!

[^16]: This is the default binding, you can set this to whatever you
       like.

[^17]: The reason for its display in the minibuffer is that the REPL
       doesn't raise itself automatically when loading files. Maybe
       you don't want it to do that. I rarely do. Often the type error
       in the minibuffer is sufficient for me to fix the error without
       having to switch buffers.

[^18]: The minibuffer in Emacs is just the little bit at the bottom
       which displays messages.

[^19]: The current indentors, both very large and complicated modules,
       try to be very general, which shows, because it's trivial to
       produce cases where they completely do the wrong thing with
       your code or go to the wrong place that you want. Half of the
       time it does what you want, i.e., the trivial case of stepping
       in or out, and the other half you're hitting tab and going
       through the tab cycle. This really distracts you when trying to
       think.

[^20]: To put things into perspective, the normal GHCi output for this
       action is:

        /home/chris/Projects/me/amelie/src/Main.hs:74:36:
            Couldn't match expected type `Chan Text'
                   against inferred type `Controller ()'
            In the first argument of `run', namely `Style.handle'
            In the expression: run Style.handle
            In the expression: ("/css/amelie.css"
                               ,run Style.handle)

[^21]: I currently don't support multi-line imports (nor will?). This
       encourages me not to have multi-line imports in my
       projects. None of them do as a result. I either import
       qualified, or just a few symbols. Or if *really* necessary,
       make another import line, one for types, one for functions. But
       I try to keep imports qualified if non-trivial. Nobody wants to
       read big import lines.

[^22]: This is quite fast, I use this on a 15K~ line project and it's
       no problem (and it is asynchronous).

[^23]: The normal output would be:

        Preprocessing executables for amelie-0.1...
        Building amelie-0.1...
        [48 of 62] Compiling Amelie.Controller.Activity ( src/Amelie/Controller/Activity.hs, dist/build/amelie/amelie-tmp/Amelie/Controller/Activity.o )
        [62 of 62] Compiling Main             ( src/Main.hs, dist/build/amelie/amelie-tmp/Main.o )
        Linking: dist/build/amelie/amelie

[^24]: For example, mine is:

         (setq hs-config-scripts '("scripts/server-restart"
                                   "../scripts/remote-refresh"
                                   "../scripts/remote-update"
                                   "../scripts/remote-prod"
                                   "../scripts/remote-push"))

[^25]: Alejandro Serrano demonstrated Eclipse's support for debugger
       with GHCi and it looked very impressive and simple to
       implement. I would like to add this to Emacs because it sounds
       nice, I don't think I would have much use for it as I rarely
       need a debugger in Haskell, but others will.

[^26]: I had a go at this but it font lock can be finicky when trying
       to have two modes in one buffer. More research is needed. Would
       appreciate help on this. I do have one idea for how to do it
       but it's not pretty (make font lock regex aware of the prompt
       string).

[^27]: I already made [a start on this](http://hpaste.org/raw/50518)
       and have a parser working for Show output. Now all that's left
       is to write the Elisp for inspecting these Show values.
