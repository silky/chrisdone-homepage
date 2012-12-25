---
date: 2011-06-05
title: PostgreSQL, hpaste, and EDSLs
description: PostgreSQL, hpaste, and EDSLs
author: Chris Done
tags: blog, haskell, hakyll
---

I'm nearing the end of a four day hacking weekend, starting at the 2nd
of June, thanks to the Festa della Repubblica in Italy.

Since then I've hacked on some things.

# pgsql-simple

I started dabbling with writing pure Haskell bindings to PostgreSQL a
few weeks ago and this weekend I decided I've give it a real
API. While thinking of how I wanted the API to be, I saw that Bryan
O’Sullivan announced
[mysql-simple](http://www.haskell.org/pipermail/haskell-cafe/2011-May/091538.html),
a fast and easy to use library for working with MySQL. I liked the API
as it resembled my prototype, and so he proposed we use a consistent
API.

So I gutted mysql-simple and replaced the MySQL bits with my
PostgreSQL
library. [The result of that is here.](https://github.com/chrisdone/pgsql-simple)

And by Jove, it works!

What I particularly like about Bryan’s API is that serialization to
and from the database is not bidirectional; there are two classes,
`Param` and `Result`, because sometimes you have data that should be
written to the database, but never read, or vise-versa. I have this
same problem with Text.JSON, but that's a different bucket of squid.

# hpaste

I thought I'd test out my new library with a simple web application,
hpaste.org. My existing codebase for it needing rewriting anyway
[“Plan to throw one away.”](http://en.wikipedia.org/wiki/The_Mythical_Man-Month#The_pilot_system)

The new one in development is
[here](https://github.com/chrisdone/amelie).

I also figured I'd try out Snap and move away from FastCGI because
it's a bit clunky.

I also thought I'd try out using
[the Model-View-Controller pattern](http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller)
properly from the start.

The main entry point to the web site is the Snap router, then I pass
everything onto the appropriate controller with a database connection
pool. The pool just maintains a list of open DB connections to avoid
the overhead of reconnecting.

    import Database.PostgreSQL.Simple (Pool,newPool)

    -- | Main entry point.
    main :: IO ()
    main = do
      p <- newPool auth
      setUnicodeLocale "en_US"
      httpServe config (serve p)
      where config = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

    -- | Serve the controllers.
    serve :: Pool -> Snap ()
    serve p = route routes where
      routes = [("/css/amelie.css", run Style.handle)
               ,("/js/amelie.js", run Script.handle)
               ,("/css/",serveDirectory "wwwroot/css")
               ,("/js/",serveDirectory "wwwroot/js")
               ,("",run Home.handle)
               ,("/:id",run Paste.handle)
               ]
      run = runHandler p

## MVC

First I came up with some
[types for the controller and model](https://github.com/chrisdone/amelie/blob/master/src/Amelie/Types/MVC.hs). I
wanted the following properties:

* The controller can request things of the model, do IO, and do web
  request stuff.
* The model can talk to the database, and do arbitrary IO, but cannot
  call the controller or do web request stuff (no redirecting, no
  header passing, etc.)
* The view can only take data provided to it by the controller and
  output text.

Thus spake the Haskell source:

    -- | The state accessible to the controller (DB/session stuff).
    data ControllerState = ControllerState {
        controllerStateConn :: Connection
      }

    -- | The controller monad.
    newtype Controller a = Controller {
        runController :: ReaderT ControllerState Snap a
      } deriving (Monad
                 ,Functor
                 ,Applicative
                 ,Alternative
                 ,MonadReader ControllerState
                 ,MonadSnap
                 ,MonadIO
                 ,MonadPlus
                 ,MonadCatchIO)

There's my controller. And my model:

    -- | The state accessible to the model (just DB connection).
    data ModelState = ModelState {
        modelStateConn :: Connection
      }

    -- | The model monad (limited access to IO, only DB access).
    newtype Model a = Model {
        runModel :: ReaderT ModelState IO a
      } deriving (Monad,Functor,Applicative,MonadReader ModelState)

The controller has the connection in its environment, but it cannot
use it directly. The model does that.

### The Controller

So, for example, I have [the home page Controller module](https://github.com/chrisdone/amelie/blob/master/src/Amelie/Controller/Home.hs) defined like so:

    import Amelie.Controller       (output)
    import Amelie.Controller.Paste (pasteForm)
    import Amelie.Model
    import Amelie.Model.Home       (getPastes)
    import Amelie.View.Home        (page)

    handle :: Controller ()
    handle = do
      pastes <- model $ getPastes
      form <- pasteForm
      output $ page pastes form

The `model` function lets us talk to the model from the
controller. The import structure is quite revealing here of what's
going on. So here the MVC parts are:

* Model: `getPastes`
* View: `page`
* Controller: `handle`, `pasteForm`

### The View

The
[view for the page](https://github.com/chrisdone/amelie/blob/master/src/Amelie/View/Home.hs)
is a pure function, taking in data and
outputting some `Html`:

    -- | Render the home page.
    page :: [Paste] -> Html -> Html
    page ps form =
      layoutPage $ Page {
        pageTitle = "λ Knights!"
      , pageBody = content ps form
      , pageName = "home"
      }

And layoutPage is another view function that just takes a Page spec
and renders a basic HTML page:

    -- | Render the page in a layout.
    layoutPage :: Page -> Html
    layoutPage Page{..} = do
      docTypeHtml $ do
        html $ do
          head $ do
            title $ toHtml $ pageTitle
         …

I thought this would be a nice way to do a simple template.

### The Model

In [`Amelie.Model.Paste`](https://github.com/chrisdone/amelie/blob/master/src/Amelie/Model/Paste.hs) I have a set of functions that only talk to
the database.

    -- | Get the latest pastes.
    getLatestPastes :: Model [Paste]
    getLatestPastes =
      queryNoParams ["SELECT *"
                    ,"FROM public_toplevel_paste"
                    ,"ORDER BY id DESC"
                    ,"LIMIT 20"]

    -- | Get a paste by its id.
    getPasteById :: PasteId -> Model (Maybe Paste)
    getPasteById pid =
      listToMaybe <$> query ["SELECT *"
                            ,"FROM public_paste"
                            ,"WHERE id = ?"]
                            (Only pid)

# CSS EDSL

I also decided that I didn't want to write in normal CSS anymore, and
that I didn't really need [Sass](http://sass-lang.com/) and those like
it when I could just embed a trivial DSL in Haskell.

I just
[encoded all CSS properties as functions](https://github.com/chrisdone/amelie/blob/master/src/Text/CSS/Properties.hs),
and then
[made a writer](https://github.com/chrisdone/amelie/blob/master/src/Text/CSS/Types.hs),
and then a
[printer and pretty printer](https://github.com/chrisdone/amelie/blob/master/src/Text/CSS.hs).

Long story short I can now write
[some nice source for my stylesheets.](https://github.com/chrisdone/amelie/blob/master/src/Amelie/View/Style.hs). Example:

    -- | Side-wide style sheet.
    style :: Text
    style = renderCSS $ runCSS $ do
      layout
      sections
      paste
      utils
      highlighter
      form
      home

    -- | Paste view styles.
    paste :: CSS Rule
    paste = do
      classRule "paste-specs" $ do
        margin "0"
        padding "0"
        listStyle "none"
        lineHeight "1.5em"

        subRule "strong" $ do
          fontWeight "normal"
          width "8em"
          display "block"
          float "left"

And so on. I defined this as a Style view.

# JavaScript EDSL

I also decided based on this trend I would go with a JavaScript DSL,
too, namely,
[HJScript](http://hackage.haskell.org/package/HJScript-0.5.0)

On the hpaste paste page, the layout of the page resizes according to
the visual width of the code, so
[I use a little jQuery to look-up the size of it and adjust the wrapper accordingly](https://github.com/chrisdone/amelie/blob/master/src/Amelie/View/Script.hs). Here's
the code:

    -- | All scripts on the site. Not much to do.
    script :: Text
    script = pack $ show $ snd $ evalHJScript $ do
      ready $ do
        each (setWidth (j ".amelie-wrap")
                       (mathMax (getWidth this' + 50) 500))
             (j ".highlighttable")

I defined this as a Script view.

# Summary

Overall I'm very happy with how pgsql-simple has worked out so far. I
haven't benchmarked it yet, but it's quite instantaneous in my
preliminary tests. It's really nice to have a pure Haskell database
library.

I'm also very happy with the decision to use an MVC architecture so
far, it has kept my code very tidy and clear. It seems like overkill
for small projects but in actuality it's great preparation for a
larger codebase.

And Haskell seems to be terribly well-suited to forcing such
separation of concerns, as in the MVC, at the type-level.

I will probably produce a simple DSL for querying and insert into the
database, similar to the CSS and JS ones. Nothing particularly
advanced at this stage, just something to avoid string manipulation.

P.S.

I also kinda implemented
[a small Formlets-ish library](https://github.com/chrisdone/amelie/blob/master/src/Text/Formlet.hs)
because I wanted to give names to my inputs, but shh, don't tell
anyone.

I also switched to using the
[highlighter](http://hackage.haskell.org/package/highlighter) package
because that's BSD3 and therefore I can make the whole Amelie project
BSD3.
