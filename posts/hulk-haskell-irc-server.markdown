---
date: 2011-01-30
title: Hulk: A Haskell IRC server
description: Hulk: A Haskell IRC server
author: Chris Done
tags: haskell, hulk, irc
---

Hulk is an working-and-work-in-progress IRC
server. [Github repo.](https://github.com/chrisdone/hulk/)

## Motivation

Last Wednesday night I whipped up
[a simple IRC server in Haskell in about four hours](https://github.com/chrisdone/hulk/tree/f1f8c662acfecbe7bb325ea1c1cda8f4284f0524).
We have been long time sick of the poor quality of the Skype Linux
implementation, which was, on the dev team, our main point of
communication. We agreed something like IRC would be good, so I
thought it would be easy in Haskell to make such a thing, and it
was; the next day we were chatting on it!

## Good Timing

I noticed that [Peteris Krumins](http://catonmat.net/) made a blog on
Thursday about
[how to write a TCP server in Haskell.](http://catonmat.net/blog/simple-haskell-tcp-server)
I thought, "That's good timing!" I also use the
[Network](hhttp://hackage.haskell.org/packages/archive/network/latest/doc/html/Network.html)
module and
[Control.Concurrent](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html),
and do pretty much everything he is demonstrating in the blog. So it
seems like Hulk is a nice "real-world" demonstration of a non-simple
TCP server in Haskell.

## Requirements

Our requirements for a server are quite narrow:

* It needs to be internal, so it has a mandatory login system (using
  the Crypto package for salted SHA1 passwords). This not optional
  right now.
* We don't care about flooding, IRC standards for nicknames,
  message length of 512 bytes, lack of a specific encoding
  support, etc. The server supports UTF8 everything.
* I will be adding history, a la Skype and MSN Messenger, which stores
  a log of all IRC activity and sends the ones of concern to you when
  you re-connect.
* I will also add OpenSSL support using
  [the HsOpenSSL package which seemingly makes the whole process trivial.](http://hackage.haskell.org/packages/archive/HsOpenSSL/0.9/doc/html/OpenSSL-Session.html#g:2)

We also pipe some feeds to it like tickets, Git commits, site issues,
etc.

## A Brief Overview

### General Haskell Projects

In the spirit with which Peteris writes I thought that I might describe
the design of the project a little bit.

    $ ls
    auth  history	  hulk.conf  README    src
    dist  hulk.cabal  LICENSE    Setup.hs  txt

It's good Haskell practice to start any project with `cabal init`
which asks you a series of questions and generates a `.cabal` file for
you. Common practice is to put source in the `src` dir, and have your
Project in a sub-directory matching the project name:

    $ ls src
    Control  Data  GeneratePass.hs	Hulk  Main.hs

Code that isn't specific to the particular project but could be used
anywhere should go in appropriate modules such as `Control.*`,
`Data.*`, etc. It occurs commonly that you will need this code in
other projects and because the dependency between these modules and
your main project's modules is only in one direction you can simply
copy the files over to your new project.

### Hulk's module hierarchy

    Control.Monad.IO
    Data.String
    Hulk
      Hulk.Auth
      Hulk.Client
      Hulk.Config
      Hulk.Event
      Hulk.Options
      Hulk.Providers
      Hulk.Server
      Hulk.Types
    Main

The first two just contain utilities that I tend to use often. The
`Main` module is the main entry point, then control goes to
`Hulk.Server` which starts listening on the right port, accepting
connections and handling/sending messages to/from clients.

### Purity vs Impurity

In order to handle messages and reply to them from clients, the
`Hulk.Client` module is used. The code in Hulk.Client is entirely
pure, and it is the bulk of the project. This is an intentional
effort. The original program I whipped up used a bunch of MVars and
was basically an imperative program, and about as confusing.

<center><img
src="http://img684.imageshack.us/img684/4484/spiderskullisland480x27.jpg"
title="Spider Skull Island from Venture Bros"></center>

Another "good practice" is for Haskell programs to be like a
well-oiled super villain base. On the edge is where all the
explosions happen, and inside is where the bad guys sit and drink Orzo
and control everything.

> <img style="float:right;margin-left:10px;margin-bottom:5px;"
> src="http://img593.imageshack.us/img593/1179/3202x.jpg"
> title="Henchmen"> Impure code is like the wreckless henchmen who
> always wreck everything, and double-cross you at every
> opportunity. Pure code is the evil genius who devises the master
> plan, tells the henchmen what to do, and keeps them in separate
> living quarters.

It's also common to put all your types into one module named `Types`,
as you tend to use types from every module and this avoids circular
dependency problems in the long run.

### Flow of the program

The main entry point to the project is in `Main`, as it should be:

    {-# OPTIONS -Wall #-}
    module Main where

    import Network
    import System.Console.CmdArgs
    import System.Posix

    import Hulk.Config  (getConfig)
    import Hulk.Options (options,optionsConf)
    import Hulk.Server  (start)
    import Hulk.Types   ()

    main :: IO ()
    main = withSocketsDo $ do
      _ <- installHandler sigPIPE Ignore Nothing
      cmdArgs options >>= getConfig . optionsConf >>= start

I initialise the sockets subsystem for Windows and then install a
handler for `SIGPIPE`, because that signal is sent in Unix when a
program attempts to write to a socket that has been closed. Both
Windows and Unix have their novel design choices. Go figure.

I'm using the
[CmdArgs library](http://hackage.haskell.org/package/cmdargs),
[tutorial here by the author, Neil Mitchell](http://neilmitchell.blogspot.com/2010/08/cmdargs-example.html), which I
am pleased is becoming part of my standard project repertoire.

I define my options merely as a way to specify the configuration file,
for now.

    {-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
    {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
    module Hulk.Options (Options
                        ,options
                        ,optionsConf) where

    import System.Console.CmdArgs

    data Options = Options
      { conf      :: FilePath
      } deriving (Show,Data,Typeable)

    options = Options
      { conf = def &= opt "hulk.conf" &= help "The config file."
      }
      &= summary "Hulk IRC Daemon (C) Chris Done 2011"
      &= help "Runs an IRC server based on the provided configuration file."

And I read the config file with
[the great ConfigFile library](http://hackage.haskell.org/package/ConfigFile):

    {-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
    module Hulk.Config
        (Config(..)
        ,getConfig)
        where

    import Data.Word
    import Data.ConfigFile

    import Hulk.Types

    getConfig :: FilePath -> IO Config
    getConfig conf = do
      contents <- readFile conf
      let config = do
            c <- readstring emptyCP contents
            hostname <- get c "LISTEN" "hostname"
            listen <- get c "LISTEN" "port"
            motd <- get c "STRINGS" "motd_file"
            preface <- get c "STRINGS" "preface_file"
            passwd <- get c "AUTH" "passwd_file"
            key <- get c "AUTH" "passwd_key"
            return Config { configListen = fromIntegral (listen::Word16)
                          , configMotd = Just motd
                          , configHostname = hostname
                          , configPasswd = passwd
                          , configPasswdKey = key
                          , configPreface = Just preface
                          }
      case config of
        Left cperr -> error $ show cperr
        Right config -> return config

The reading process is merely a simple monad that either returns the
`Config` object or an error. I choose to just throw an error when
there's an issue. I use this library for pretty much every project I
use, it really is an essential library.

An alternative way to express the code above is to `runReaderT`,
define a function like `get' = ask >>= flip get` and then you can
express the above with Applicative operators `(<$>)` and `(<*>)`.

This module will read a file like this:

    [LISTEN]
    port = 6667
    hostname = cn-done

    [STRINGS]
    motd_file = txt/MOTD
    preface_file = txt/PREFACE

    [AUTH]
    passwd_file = auth/passwd

### Server starter

I start the server by using the `listenOn` function, covered in
Peteris's post, and accept connections, setting the buffering to
`NoBuffering`. This turns out to be rather important; as Peteris
mentions, this avoids surprises with buffering, which is something I
experienced when testing out `LineBuffering` in this project. In
certain situations unknown to me, access to handles locks up.

    -- | Start an IRC server with the given configuration.
    start :: Config -> IO ()
    start config = withSocketsDo $ do
      hSetBuffering stdout LineBuffering
      listenSock <- listenOn $ PortNumber (configListen config)
      envar <- newMVar Env { envClients = M.empty
                           , envNicks = M.empty
                           , envChannels = M.empty }
      forever $ do
        (handle,host,_port) <- accept listenSock
        hSetBuffering handle NoBuffering
        let conn = Conn { connRef = newRef handle
                        , connHostname = host
                        , connServerName = configHostname config
                        }
        _ <- forkIO $ handleClient config handle envar conn
        return ()

### Connection handling

I fork a new thread per handle. No big deal. I have one value,
`envar`, of type `MVar Env`, which stores the state of the whole
server. It can only be accessed by one thread at a time, that's why I
put it on an `MVar`. The definition of `Env` is:

    data Env = Env {
       envClients :: Map Ref Client
      ,envNicks :: Map Nick Ref
      ,envChannels :: Map ChannelName Channel
    }

where

    newtype Ref = Ref { unRef :: Handle }
        deriving (Show,Eq)

`Ref` is merely a wrapper for Handles, to avoid me accidentally using a
handle. It is only used as a unique reference to a client.

In the new thread I have a handler which receives newlines from the
client:

    -- | Handle a client connection.
    handleClient :: Config -> Handle -> MVar Env -> Conn -> IO ()
    handleClient config handle env conn = do
      let runHandle = runClientHandler config env handle conn
      runHandle $ makeLine CONNECT []
      fix $ \loop -> do
        line <- catch (Right <$> UTF8.hGetLine handle) (return . Left)
        case filter (not.newline) <$> line of
          Right []   -> loop
          Right line -> do runHandle (line++"\r"); loop
          Left _err  -> runHandle $ makeLine DISCONNECT ["Connection lost."]

      where newline c = c=='\n' || c=='\r'

I get a line which is Right, or fail and return what's Left. The case
of getLine failing is when the socket is closed. I ignore messages
only containing newline characters, and the middle case is actually
getting a valid line which I pass to `runHandle` that runs the pure
client handler, then `loop`s again.

### Client handler

To run the client handler, I have the following function:

    -- | Handle a received line from the client.
    runClientHandler :: Config -> MVar Env -> Handle -> Conn -> String -> IO ()
    runClientHandler config env handle conn line = do
      modifyMVar_ env $ \env -> do
        (replies,env) <- runReaderT (runHulkIO $ handleLine env conn line) config
        mapM_ (handleReplies handle) replies
        return env

It passes the program state (`env`) and the current connection info
(`conn`) to the function `handleLine`, which is the single export from
`Hulk.Client`, which is a transformer over an arbitrary
monad. Technically, in this case I'm running it inside a `readerT` on
`IO`, so it's not actually pure. The `handleLine` action returns a
bunch of replies/instructions for the `Server` module to perform and a
new state (`env`).

When I said that the `Hulk.Client` module was pure, I meant that it is
abstracted over whether it is pure or impure, and therefore can be
treated as pure for testing and developing, and when running the
server, runs in IO, but only 0.1% of the code uses IO. Also, when I
said "arbitrary monad", I meant any monad implementing the
`MonadProvider` class.

    class Monad m => MonadProvider m where
      providePreface   :: m (Maybe String)
      provideMotd      :: m (Maybe String)
      provideKey       :: m String
      providePasswords :: m String

Meaning that these are the only "impure" things I need when running
the program. I need to read the preface, motd, key, and password files
on demand. In the `IO` case, I simply read the file. In the pure case,
I can stick it in a `Reader` or `Identity` monad and the whole
computation is thus pure.

What's the benefit? This means I can run arbitrary parts of the
computation trivially, and make pure test suites out of it. QuickCheck
my IRCd, anyone? The main benefits are not to have to worry about
conflicting simultaneous threads, and being able to run any function
from the module with whatever state one desires.

### Client replies

The `Client` module replies with one of the following:

    data Reply = MessageReply Ref Message | LogReply String | Close

* `MessageReply`: Send this `Message` to the given handle (`Ref`).
* `LogReply`: Log this `String`.
* `Close`: Close the current connection.

I find this separation of IO and logic to be useful.

### The IRC monad stack

The rest of the project lies in `Hulk.Client` and is
academic/straight-forward. I will explain the `IRC` monad, though:

    newtype IRC m a = IRC {
        runIRC :: ReaderT Conn (WriterT [Reply] (StateT Env m)) a
      }
      deriving (Monad
               ,Functor
               ,MonadWriter [Reply]
               ,MonadState Env
               ,MonadReader Conn)

* I output replies, hence the `MonadWriter [Reply]`.
* I read the connection info (`Conn`), but I *don't/shouldn't* modify it
  or write to it.
* I *do* read/modify the `Env`, which is the whole server state.
* Finally, it is parametrized over an arbitrary monad, but the
  functions in `Client` constrain to `MonadProvider`.

This is called
[a monad transformer stack](http://book.realworldhaskell.org/read/monad-transformers.html). [Haskell Wikibook on transformers](http://en.wikibooks.org/wiki/Haskell/Monad_transformers)

### Examples

Example of a `MessageReply`:

    -- | Send a message reply.
    reply :: Monad m => Ref -> Message -> IRC m ()
    reply ref msg = do
      outgoing $ encode msg
      tell . return $ MessageReply ref msg

Examples of `LogReply`:

    -- | Log an outgoing line.
    outgoing :: Monad m => String -> IRC m ()
    outgoing = log . ("-> " ++)

    -- | Log a line.
    log :: Monad m => String -> IRC m ()
    log = tell . return . LogReply

Example of the `StateT`:

    -- | Modify the nicks mapping.
    modifyNicks :: Monad m => (Map Nick Ref -> Map Nick Ref) -> IRC m ()
    modifyNicks f = modify $ \env -> env { envNicks = f (envNicks env) }

Example of using the `Conn` object from the `ReaderT`:

    -- | Make a new IRC message from the server.
    newServerMsg :: Monad m => String -> [String] -> IRC m Message
    newServerMsg cmd ps = do
      hostname <- asks connServerName
      return $ Message {
        msg_prefix = Just $ Server hostname
       ,msg_command = cmd
       ,msg_params = ps
      }

## Summary

That's all, folks! I hope this is useful to some people thinking of
writing their first Haskell daemon project.

Haskell is the only language I know in which I can write 400~ lines of
code without running it and then run it and have it work as expected.
