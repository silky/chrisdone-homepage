{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Category
import Data.List
import Data.Monoid
import Data.Ord
import Hakyll
import Hakyll.Web.Page.Metadata
import Prelude hiding (id,(.))

main :: IO ()
main = hakyll $ do
    -- Images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Comics
    match "images/comics/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ""
        compile $ pageCompiler
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> arr (copyBodyToField "description")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "archive" $ route idRoute
    create "archive" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (id *** arr dateOrdered >>> addPostList)
        >>> renderTagsField "prettytags" (fromCapture "tags/*")
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index" $ route $ idRoute
    create "index" $ constA mempty
        >>> arr (setField "title" "Chris Done's Homepage")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA "posts/*" (id *** arr (reverse . take 5 . reverse . dateOrdered) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ""
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
            >>> arr dateOrdered
            >>> arr reverse
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 200

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"


-- | Sort pages by their date field.
dateOrdered :: [Page a] -> [Page a]
dateOrdered = sortBy (comparing (getField "date"))

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr reverse
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- | Make a list of posts given a tag.
makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Chris Done's home page feed."
    , feedDescription = "Chris Done's home page feed."
    , feedAuthorName  = "Chris Done"
    , feedAuthorEmail = "chrisdone@gmail.com"
    , feedRoot        = "http://chrisdone.com"
    }
