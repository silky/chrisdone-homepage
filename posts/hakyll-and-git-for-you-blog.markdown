---
date: 2010-04-04
title: Write your blog with Hakyll and Git
description: Write your blog with Hakyll and Git
author: Chris Done
tags: haskell, hakyll, git, blog
---

My home page and blog is written in
[Markdown](http://daringfireball.net/projects/markdown/) and built
with [Hakyll](http://jaspervdj.be/hakyll/). It’s hosted on a Linode
VPS account and I update it with [Git](http://git-scm.com/). Here’s
how I do it.

## Work locally

I have my local web site directory:

    chris@chrisamilo:~/Projects/chrisdone-homepage$ ls
    code  css  hakyll.hs  images  index.html  posts  posts.html  README
    templates

Which is mostly stolen from [Jasper Van der Jeugt’s Hakyll blog
example](http://jaspervdj.be/hakyll/tutorials/part06.html)

I work on it locally by building the Hakyll program:

    chris@chrisamilo:~/Projects/chrisdone-homepage$ ghc --make hakyll
    [1 of 1] Compiling Main             ( hakyll.hs, hakyll.o )
    Linking hakyll ...

And previewing it locally:

    chris@chrisamilo:~/Projects/chrisdone-homepage$ ./hakyll preview
    Rendering _site/posts.html
    Rendering _site/tags/blog.html
    Rendering _site/tags/emacs.html
    Rendering _site/tags/hakyll.html
    Rendering _site/tags/haskell.html
    Rendering _site/tags/twitter.html
    Rendering _site/index.html
    Rendering _site/posts/2010-04-04-tweet-your-haskell-errors.html
    Rendering _site/posts/2010-04-04-hakyll-and-a-new-blog.html
    Rendering _site/rss.xml
    Starting hakyll server on port 8000...

`hakyll preview` watches your directories for changes and renders any updated
files and saves them in the `_site` directory.

## Push to remote


Once I’m happy with all my site/blog changes, I commit them:

    chris@chrisamilo:~/Projects/chrisdone-homepage$ git commit -m "added code directory, post about tweeting your haskell errors"
    [master 02bc308] added code directory, post about tweeting your haskell errors
     4 files changed, 166 insertions(+), 0 deletions(-)
     create mode 100644 code/haskell-tweet-errors.el
     create mode 100644 images/tweet-haskell-errors.png
     create mode 100644 posts/2010-04-04-tweet-your-haskell-errors.markdown
    chris@chrisamilo:~/Projects/chrisdone-homepage$ git status
    # On branch master
    nothing to commit (working directory clean)

Then I (personally) push to github, just because I like looking at
projects on Github.

    chris@chrisamilo:~/Projects/chrisdone-homepage$ git push github
    Counting objects: 18, done.
    Delta compression using up to 4 threads.
    Compressing objects: 100% (13/13), done.
    Writing objects: 100% (13/13), 34.59 KiB, done.
    Total 13 (delta 3), reused 0 (delta 0)
    To git@github.com:chrisdone/chrisdone-homepage.git
       62836bf..02bc308  master -> master

Finally I push to my Linode server, which pushes the commit, then runs
the post-receive hook which recompiles the `hakyll` program, and runs
`./hakyll build`, which updates the changed files in the `_site`
directory (which is really just a softlink to my
`/var/www/chrisdone.com` directory.)

    chris@chrisamilo:~/Projects/chrisdone-homepage$ git push linode
    Counting objects: 18, done.
    Delta compression using up to 4 threads.
    Compressing objects: 100% (13/13), done.
    Writing objects: 100% (13/13), 34.59 KiB, done.
    Total 13 (delta 3), reused 0 (delta 0)
    [1 of 1] Compiling Main             ( hakyll.hs, hakyll.o )
    Linking hakyll ...
    Rendering _site/css/default.css
    Rendering _site/images/tweet-haskell-errors.png
    Rendering _site/code/haskell-tweet-errors.el
    Rendering _site/posts.html
    Rendering _site/tags/emacs.html
    Rendering _site/tags/haskell.html
    Rendering _site/tags/twitter.html
    Rendering _site/index.html
    Rendering _site/posts/2010-04-04-tweet-your-haskell-errors.html
    Rendering _site/rss.xml
    To chris@chrisdone.com:chrisdone-homepage.git
       62836bf..02bc308  master -> master
    chris@chrisamilo:~/Projects/chrisdone-homepage$

And that’s it!

## My configuration

`.gitignore`:

    _cache
    _site
    *~
    *.hi
    *.o
    hakyll

`.git/config`:

    [remote "github"]
            url = git@github.com:chrisdone/chrisdone-homepage.git
            fetch = +refs/heads/*:refs/remotes/origin/*
            push = +master:refs/heads/master
    [remote "linode"]
            url = chris@chrisdone.com:chrisdone-homepage.git
            push = +master:refs/heads/master

I never fetch from the Linode server, just push to it, so I don’t
include a fetch configuration key.

The remote repository can just be empty:

    $ mkdir chrisdone-homepage.git
    $ cd chrisdone-homepage.git
    $ git init
    Initialized empty Git repository in /home/chris/chrisdone-homepage.git

And then in my `chrisdone-homepage.git/.git/hooks/post-receive` file:

    #!/bin/sh
    if [ -n $GIT_DIR ]; then
    unset GIT_DIR
    cd ..
    fi
    git checkout -f
    ghc --make hakyll
    ./hakyll build

I had to look this one up. Git uses `GIT_DIR` to overwrite the current
pwd, so you have to unset it, and the pwd starts as
`$YOURPROJECT/.git`, so you have to `cd ..`. Once that’s done you just
`git checkout -f`, which does the following:

    -f
        Proceed even if the index or the working tree differs from HEAD.
        This is used to throw away local changes.

Then we build `hakyll` and update the `_site` directory!
