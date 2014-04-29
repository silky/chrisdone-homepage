---
date: 2014-04-29
title: The Identity monad trick
description: The Identity monad trick
author: Chris Done
tags: haskell
---

I heard about this from John Wiegley a while ago, but every time I
recall it, I can't remember how it goes, so I thought I'd write it
down for myself. I think there's a paper about it, but I can't find
it. Hopefully I'm recalling it correctly.

The Identity monad trick: Let's say I want to expose an API that lets
you work with a data structure. I want you to be able to keep hold of
that data structure and pass it back into my library, and I'll give it
back to you later and we can go back and forth.

**But** I don't want you to actually *give you* the data structure
freely so you can go and give it to your friends. So instead I force
you into the Identity monad, via a newtype wrapper that only *I* can
unpack.

``` haskell
newtype Secret a = Secret { unSecret :: Identity a }
  deriving (Monad,Functor,Applicative)
``

And I have some function exposing it like:

``` haskell
getSecret :: Foo -> Secret Text
```

Here, use that. What can you do with it? You can't extract the value
out, you can only compose it with more functor or monad stuff:

``` haskell
fmap (map T.toUpper) (getSecret foo)
```

Or:

``` haskell
do text <- getSecret foo
   if all T.isUpper text
      then return (T.reverse text)
      else return text
```

You've used the value, but it never escaped[^1] the actual Identity
monad. It's like I'm giving you the value, but I'm also not giving you
the value.

[^1]: As always, bottom complicates it, so you should force it in the
      IO monad and catch any exceptions e.g.

      ``` haskell
      extract :: Secret a -> IO (Maybe a)
      ```

      This prevents people from using

      ``` haskell
      (v >>= \a -> error ("The value is " ++ show a))`
      ```

      To try to get around it. `unsafePerformIO` can get around it,
      but maybe you have control over whether people can import that.
