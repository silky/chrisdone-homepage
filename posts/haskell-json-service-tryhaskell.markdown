---
date: 2010-04-05
title: A Haskell JSON service and updated Try Haskell!
author: Chris Done
tags: blog, haskell, tryhaskell, ajax
---

**UPDATE:** The service went down 5 April at about 19:50 GMT+1 to
  about 20:21. I had to update the watchdog to be more aggressive for
  Linode (as opposed to my development machine). Just letting you guys
  know.

## Make your own Try Haskell!

You can now run [Try Haskell!](http://tryhaskell.org/) as a simple
HTML file locally, or on your own domain, if you like! All you need is
an internet connection.

Grab the source with git (or download a
[.zip](http://github.com/chrisdone/tryhaskell/zipball/master) or
[.tar.gz](http://github.com/chrisdone/tryhaskell/tarball/master) of
the latest version):

    $ git clone git://github.com/chrisdone/tryhaskell.git

Get writing interactive tutorials! Please let me know how you get on.

## Evaluate Haskell code anywhere

You can now also use the JSONP service that Try Haskell uses from
anywhere!

It is located at: `http://tryhaskell.org/haskell.json`

And you can download [the haskell-json source from
Github](http://github.com/chrisdone/haskell-json):

    $ git clone git://github.com/chrisdone/haskell-json.git

Here is a simple Haskell evaluating text box. [Download the
source.](/code/haskell-json-example.zip)

   <form action="" id="evaluator">
     <fieldset>
       <legend>Haskell evaluation</legend>
       <label>Expression:</label>
       <input type="text" id="expr"/>
       <input type="submit" value="Evaluate"/>
       <p>Result: <code id="eval-output">Output here.</code></p>
       <p>Type: <code id="eval-type">Type here.</code></p>
     </fieldset>
   </form>

   <form action="" id="loader">
     <fieldset>
       <legend>Haskell loading</legend>
       <label>Code:</label>
       <textarea id="contents"></textarea>
       <input type="submit" value="Load code"/>
       <p>Result: <code id="load-output">Output here.</code></p>
     </fieldset>
   </form>

   <script type="text/javascript" src="/code/jquery-1.4.2.min.js"></script>
   <script type="text/javascript" src="/code/haskell-json/encode-hex.js"></script>
   <script type="text/javascript" src="/code/haskell-json/haskell-json.js"></script>

Feel free to make this pretty and use it in your blog or books,
Haskell courses, etc. Trying Haskell should be easy!

## Methods and options

### `eval` method

There is an **eval** method for evaluating expressions, used like this:

    http://tryhaskell.org/haskell.json?method=eval&expr=23*52

and you get back:

    {"result":"1196","type":"(Num t) => t","expr":"23*52"}

Note that computations are limited to 750ms. This limit may be
softened if people have problems doing useful things.

### `load` method

And there is a **load** method for loading your own file into the
service for later evaluation, used like this:

    http://tryhaskell.org/haskell.json?method=load&contents=x=1

and you get back:

    {"success":"wrote file."}

Then you can request

    http://tryhaskell.org/haskell.json?method=eval&expr=x

and get back:

    {"result":"5","type":"Integer","expr":"x"}

Hurrah!

### `guid` for unique states

If you are writing a tutorial or a blog post, maybe you want your
visitors to be able to maintain two different states depending on what
post/chapter they have open in what tab. You can do this by adding a
guid to your request:

    http://tryhaskell.org/haskell.json?method=load&contents=x=1&guid=ch2

For "chapter2", or maybe you could use the ID of your blog post, or
whatever you want, with the condition that it is **alphanumeric.**

### `pad` or "JSONP"

If you are making your request from JavaScript, you will need to follow
the JSON style of making requests. `haskell.json` supports this by
adding a `pad` parameter:

    http://tryhaskell.org/haskell.json?method=eval&expr=Just%201&pad=doStuff

and you get back:

    doStuff({"result":"Just 1","type":"(Num t) => Maybe t","expr":"Just 1"})

## Implementing simple JSONP in JavaScript

I use jQuery in these examples, because it is really easy.

So for example I wrote a little function for tryhaskell for making
JSONP requests:

    function jsonp(url,func) {
        handleJSON = function(r){
            script.remove();
            func(r);
        };
        var script = $('<script type="text/javascript" src="'+url+'"></script>');
        script.attr('src',url);
        $('body').append(script);
    }

And it references a global `handleJSON` function that I define at the
top level so that any script file could access it:

    var handleJSON = function(a){ alert('Unassigned JSONP: ' + a); }

(Just have a little alert message in case we get something wrong.)

It adds a script tag to the body with the URL we want to query. This
is how it gets around the domain restriction of normal AJAX. It is kind
of an ugly solution if you ask me, but there you go!

Once the script has been loaded, the global function `handleJSON` is
called. So before that happens, we need to overwrite it `handleJSON`
with our closure to remove our `script` tag from the DOM and then call
the function provided to `jsonp`.

Bringing it all together we can write something like this:

    jsonp("http://tryhaskell.org/haskell.json?method=eval&pad=handleJSON&expr="
           + encodeHex("34*23"),
          function(resp){
             alert("Result: " + resp.result);
          });

Encoding what you provide in the URL is important.

See the sample zip file above for an example of this.
