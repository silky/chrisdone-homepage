---
date: 2014-09-21
title: shell-conduit: Write shell scripts in Haskell with Conduit
description: shell-conduit: Write shell scripts in Haskell with Conduit
author: Chris Done
tags: haskell, conduit
---

As part of my series of write-about-personal-projects, my latest
obsession is writing shell scripts with Michael Snoyman's
[Conduit](http://hackage.haskell.org/package/conduit).

Here is my package,
[shell-conduit](http://github.com/chrisdone/shell-conduit/). It's
still in the experimental phase, but I don't forsee any changes now
for a while.

## Bash is evil

I hate writing scripts in Bash. Until now, it was the easiest way to
just write unix scripts. Its syntax is insane, incredibly error prone,
its defaults are awful, and it's not a real big person programming
language.

## Perl/Python/Ruby are also evil

If you're going to go as far as using a real programming language, why
bother with these dynamically typed messes? Go straight for Haskell.

## Like a glove

I had an inkling a while back that conduits mirror the behaviour of
bash pipes very well. I knew there was something to the idea, but
didn't act on it fully for a while. Last week I experimented somewhat
and realised that the following Haskell code

``` haskell
source $= conduit $$ sink
```

does indeed accurately mirror

``` bash
source | conduit > sink
```

And that also the following

``` haskell
(do source
    source $= conduit)
$$$$ sink
```

is analogous to

``` bash
source
source | conduit
```

We'll see examples of why this works later.

## I must Haskell all the things

Another trick I realised is to write some template Haskell code which
will calculate all executables in your PATH at compilation time and
generate a top-level name that is a Haskell function to launch that
process. So instead of writing

``` haskell
run "ls"
```

you could instead just write

``` haskell
ls
```

There are a few thousand executables, so it takes about 10 seconds to
compile such a module of names. But that's all.

Again, we'll see how awesome this looks in a minute.

## Modeling stdin, stderr and stdout

My choice of modeling the typical shell scripting pipe handles is by
having a type called `Chunk`:

``` haskell
type Chunk = Either ByteString ByteString
```

All `Left` values are from `stderr`. All `Right` values are either
being pulled from `stdin` or being sent to `stdout`. In a conduit the
difference between `stdin` and `stdout` is more conceptual than real.

When piping two commands, the idea is that any `Left` values are just
re-yielded along, they are not consumed and passed into the process.

## A process conduit on chunks

Putting the previous model into practice, we come up with a type for
launching a process like this:

``` haskell
conduitProcess :: (MonadResource m)
               => CreateProcess -> Conduit Chunk m Chunk
```

Meaning the process will be launched, and the conduit will accept any
upstream `stdin` (`Right` values), and send downstream anything that
comes from the actual process (both `Left` and `Right` values).

## Process conduits API

I defined two handy functions for running process conduits:

``` haskell
shell :: (MonadResource m)
      => String -> Conduit Chunk m Chunk
proc :: (MonadResource m)
     => String -> [String] -> Conduit Chunk m Chunk
```

One to launch via a shell, one to launch via program name and
arguments. These functions can be used in your shell scripts. Though,
we'll see in a minute why you should rarely need either.

## Executing a shell scripting conduit

First we want something to consume any remainder chunks after a script
has finished. That's `writeChunks`:

``` haskell
writeChunks :: (MonadIO m)
            => Consumer Chunk m ()
writeChunks =
  awaitForever
    (\c ->
       case c of
         Left e -> liftIO (S.hPut stderr e)
         Right o -> liftIO (S.hPut stdout o))
```

This simply consumes anything left in the pipeline and outputs to the
correct file handles, either `stderr` or `stdout`.

Now we can write a simple `run` function:

``` haskell
run :: (MonadIO m,MonadBaseControl IO m)
    => Conduit Chunk (ShellT m) Chunk -> m ()
run p =
  runResourceT
    (runShellT (sourceList [] $=
                p $$
                writeChunks))
```

First it yields an empty upstream of chunks. That's the source. Then
our script `p` is run as the conduit in between, finally we write out
any chunks that remain.

Let's try that out:

``` haskell
λ> run (shell "echo hello!")
hello!
λ> run (proc "date" ["+%Y"])
2014
λ> run (shell "echo oops > /dev/stderr")
oops
```

Looks good. Standard output was written properly, as was stderr.

## Returning to our mass name generation

Let's take our earlier work of generating names with
template-haskell. With that in place, we have a process conduit for
every executable in `PATH`. Add to that variadic argument handling for
each one, we get a list of names like this:

``` haskell
rmdir :: ProcessType r => r
ls :: ProcessType r => r
egrep :: ProcessType r => r
dmesg :: ProcessType r => r
```

The real types when instantiated will look like:

``` haskell
rmdir "foo" :: Conduit Chunk m Chunk
ls :: Conduit Chunk m Chunk
ls "." :: Conduit Chunk m Chunk
```

## Putting it all together

We can now provide any number of arguments:

``` haskell
λ> run ls
dist
LICENSE
README.md
Setup.hs
shell-conduit.cabal
src
TAGS
TODO.org
λ> run (ls "/")
bin
boot
cdrom
…
```

We can pipe things together:

``` haskell
λ> run (do ls "-1" $= head' "-2")
dist
LICENSE
λ> run (ls $= grep "Key" $= shell "cat" $= CL.map (second (S8.map toUpper)))
KEYBOARD.HI
KEYBOARD.HS
KEYBOARD.O
```

Results are outputted to stdout unless piped into other processes:

``` haskell
λ> run (do shell "echo sup"; shell "echo hi")
sup
hi
λ> run (do shell "echo sup"; sed "s/u/a/"; shell "echo hi")
sup
hi
λ> run (do shell "echo sup" $= sed "s/u/a/"; shell "echo hi")
sap
hi
```

Live streaming between pipes like in normal shell scripting is
possible:

``` haskell
λ> run (do tail' "/tmp/example.txt" "-f" $= grep "--line-buffered" "Hello")
Hello, world!
Oh, hello!
```

(Remember that `grep` needs `--line-buffered` if it is to output things
line-by-line).

## Error handling

By default, if a process errors out, the whole script ends. This is
contrary to Bash, which keeps going regardless of failure. This is
bad.

In Bash, to revert this default, you run:

``` bash
set -e
```

And the way to ignore erroneous commands on case-by-case basis is to
use `|| true`:

``` haskell
killall nonexistant || true
echo OK, done.
```

Which means “do foo, or otherwise ignore it, continue the script”.

We can express the same thing using the Alternative instance for the
`ShellT` type:

``` haskell
λ> run (do killall "nonexistant" "-q"; echo "OK, done.")
*** Exception: ShellExitFailure 1
λ> run (do killall "nonexistant" "-q" <|> return (); echo "OK, done.")
OK, done.
```

## String types

If using `OverloadedStrings` so that you can use `Text` for arguments,
then also enable `ExtendedDefaultRules`, otherwise you'll get
ambiguous type errors.

``` haskell
{-# LANGUAGE ExtendedDefaultRules #-}
```

But this isn't necessary if you don't need to use `Text` yet. Strings
literals will be interpreted as `String`. Though you can pass a value
of type `Text` or any instance of `CmdArg` without needing conversions.

## Examples of script files

Quick script to reset my keyboard (Linux tends to forget these things
when I unplug my keyboard):

``` haskell
import Data.Conduit.Shell
main =
  run (do xmodmap ".xmodmap"
          xset "r" "rate" "150" "50")
```

Cloning and initializing a repo (ported from a bash script):

``` haskell
import Control.Monad.IO.Class
import Data.Conduit.Shell
import System.Directory
main =
  run (do exists <- liftIO (doesDirectoryExist "fpco")
          if exists
             then rm "fpco/.hsenvs" "-rf"
             else git "clone" "git@github.com:fpco/fpco.git"
          liftIO (setCurrentDirectory "fpco")
          shell "./dev-scripts/update-repo.sh"
          shell "./dev-scripts/build-all.sh"
          alertDone)
```

Script to restart a web process (ported from an old bash script I
had):

``` haskell
import Control.Applicative
import Control.Monad.Fix
import Data.Conduit.Shell
main =
  run (do ls
          echo "Restarting server ... ?"
          killall name "-q" <|> return ()
          fix (\loop ->
                 do echo "Waiting for it to terminate ..."
                    sleep "1"
                    (ps "-C" name $= discardChunks >> loop) <|> return ())
          shell "dist/build/ircbrowse/ircbrowse ircbrowse.conf")
  where name = "ircbrowse"
```

## You've seen Shelly, right?

Right. Shelly's fine. It just lacks the two killer things for me:

* All names are bound, so I can just use them as normal functions.
* shell-conduit also, due to its mass name binding, prioritizes
  commands. For example, Shelly has
  [a group of functions for manipulating the file system](http://hackage.haskell.org/package/shelly-1.5.5/docs/Shelly.html#g:9). In
  shell-conduit, you just use your normal commands: `rm "x"` and `mv
  "x" "y"`.
* Not based on conduit. Conduit is a whole suite of streaming
  utilities perfect for scripting.
* Piped is not the default, either. There're a bunch of choices: Shelly,
  Shelly.Lifted, Shelly.Pipe. Choice is good, but for a scripting
  language I personally prefer one goto way to do something.

## Conduits as good scripting libraries

You might want to import the regular Conduit modules qualified, too:

``` haskell
import qualified Data.Conduit.List as CL
```

Which contains handy functions for working on streams in a
list-like way. See the rest of the handy modules for Conduit in
[conduit-extra](http://hackage.haskell.org/package/conduit-extra).

Also of interest is
[csv-conduit](http://hackage.haskell.org/package/csv-conduit),
[html-conduit](http://hackage.haskell.org/package/html-conduit), and
[http-conduit](http://hackage.haskell.org/package/http-conduit).

Finally, see the Conduit category on Hackage for other useful
libraries: <http://hackage.haskell.org/packages/#cat:Conduit>

All of these general purpose Conduits can be used in shell
scripting.

## Using it for real scripts

So far I have ported a few small scripts to shell-conduit from Bash and
have been happy every time. I suck at Bash. I'm pretty good at
Haskell.

The next test is applying this to my Hell shell and seeing if I can
use it as a commandline shell, too.

My friend complained that having to quote all arguments is a pain. I
don't really agree that this is bad. In Bash it's often unclear how
arguments are going to be interpreted. I'm happy just writing
something predictable than something super convenient but possibly
nonsense.

## Summary

I set out a week ago to just stop writing Bash scripts. I've written a
bunch of scripts in Haskell, but I would still write Bash scripts
too. Some things were just too boring to write. I wanted to commit to
Haskell for scripting. Today, I'm fairly confident I have a solution
that is going to be satisfactory for a long while.
